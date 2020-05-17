;;; ctrlf.el --- Emacs finally learns how to ctrl+F -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 23 Dec 2019
;; Homepage: https://github.com/raxod502/ctrlf
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; Version: 1.0

;;; Commentary:

;; CTRLF (pronounced "control F") is an intuitive and efficient
;; solution for single-buffer text search in Emacs, replacing packages
;; such as Isearch, Swiper, and helm-swoop. Taking inspiration from
;; the widely-adopted and battle-tested ctrl+F interfaces in programs
;; such as web browsers, but following the flow and keybindings of
;; Isearch, CTRLF improves on existing text search solutions in
;; convenience, robustness, and consistency.

;; Please see https://github.com/raxod502/ctrlf for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'hl-line)
(require 'map)
(require 'subr-x)
(require 'thingatpt)

;;;; Backports

;; Not defined before Emacs 27.1
(eval-and-compile
  (unless (fboundp 'xor)
    (defun xor (cond1 cond2)
      "Return the boolean exclusive-or of COND1 and COND2.
If only one of the arguments is non-nil, return it; otherwise
return nil."
      (declare (pure t) (side-effect-free error-free))
      (cond ((not cond1) cond2)
            ((not cond2) cond1)))))

;;;; User configuration

(defgroup ctrlf nil
  "More streamlined replacement for Isearch, Swiper, etc."
  :group 'convenience
  :prefix "ctrlf-"
  :link '(url-link "https://github.com/raxod502/ctrlf"))

;;;;; User options

(defcustom ctrlf-highlight-current-line t
  "Non-nil means to highlight the entire line of the current match."
  :type 'boolean)

(defcustom ctrlf-auto-recenter nil
  "Non-nil means to always keep the current match vertically centered."
  :type 'boolean)

(defcustom ctrlf-show-match-count-at-eol t
  "Non-nil means to show the match count also directly in the buffer.
Otherwise, the match count is only shown in the minibuffer."
  :type 'boolean)

(defcustom ctrlf-style-alist
  '((literal      . (:prompt "literal"
                             :translator regexp-quote
                             :case-fold ctrlf-no-uppercase-literal-p))
    (regexp       . (:prompt "regexp"
                             :translator identity
                             :case-fold ctrlf-no-uppercase-regexp-p))
    (fuzzy        . (:prompt "fuzzy"
                             :translator ctrlf-translate-fuzzy-literal
                             :case-fold ctrlf-no-uppercase-literal-p))
    (fuzzy-regexp . (:prompt "fuzzy regexp"
                             :translator ctrlf-translate-fuzzy-regexp
                             :case-fold ctrlf-no-uppercase-regexp-p)))
  "Alist of CTRLF search styles.
Each search style defines a different way to interpret your
query, for example as a literal string or as a regexp. The keys
are unique identifying symbols which can be passed to
`ctrlf-forward' and `ctrlf-backward'. The values are property
lists with the following keys (all mandatory):
- `:prompt': string to be displayed in minibuffer prompt after
  \"CTRLF\".
- `:translator': function which takes your query string and
  returns a regexp, e.g. `regexp-quote' for a literal search.
- `:case-fold': function which takes your query string and
  returns a value for `case-fold-search' to use by default (for
  example, non-nil only if the query does not contain any
  uppercase letters)."
  :type '(alist
          :key-type symbol
          :value-type (list (const :prompt) string
                            (const :translator) function
                            (const :case-fold) function)))

;;;###autoload
(defcustom ctrlf-mode-bindings
  '(([remap isearch-forward        ] . ctrlf-forward-literal)
    ([remap isearch-backward       ] . ctrlf-backward-literal)
    ([remap isearch-forward-regexp ] . ctrlf-forward-regexp)
    ([remap isearch-backward-regexp] . ctrlf-backward-regexp))
  "Keybindings enabled in `ctrlf-mode'. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
`ctrlf-mode' is (re-)enabled. The keys are strings or raw key
events and the values are command symbols.

These bindings are available globally in Emacs. See also
`ctrlf-minibuffer-bindings', which defines bindings that are
active in the minibuffer during a search."
  :type '(alist
          :key-type sexp
          :value-type function)
  :set (lambda (var val)
         (set var val)
         (when (bound-and-true-p ctrlf-mode)
           (ctrlf-mode +1))))

(defcustom ctrlf-minibuffer-bindings
  '(([remap abort-recursive-edit]           . ctrlf-cancel)
    ;; This is bound in `minibuffer-local-map' by loading `delsel', so
    ;; we have to account for it too.
    ([remap minibuffer-keyboard-quit]       . ctrlf-cancel)
    ;; Use `minibuffer-beginning-of-buffer' for Emacs >=27 and
    ;; `beginning-of-buffer' for Emacs <=26.
    ([remap minibuffer-beginning-of-buffer] . ctrlf-first-match)
    ([remap beginning-of-buffer]            . ctrlf-first-match)
    ([remap end-of-buffer]                  . ctrlf-last-match)
    ([remap scroll-up-command]              . ctrlf-next-page)
    ([remap scroll-down-command]            . ctrlf-previous-page)
    ([remap recenter-top-bottom]            . ctrlf-recenter-top-bottom)
    ;; Reuse transient binding of `isearch-toggle-case-fold'.
    ("M-s c"     . ctrlf-toggle-case-fold-search)
    ("C-o c"     . ctrlf-toggle-case-fold-search)
    ;; Reuse transient binding of `isearch-toggle-regexp'.
    ("M-s s"     . ctrlf-change-search-style)
    ("C-o s"     . ctrlf-change-search-style))
  "Keybindings enabled in minibuffer during search. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
entering the minibuffer. The keys are strings or raw key events
and the values are command symbols. The keymap so constructed
inherits from `minibuffer-local-map'.

See also `ctrlf-mode-bindings', which defines bindings that are
available globally in Emacs when `ctrlf-mode' is active."
  :type '(alist
          :key-type sexp
          :value-type function))

(defcustom ctrlf-zero-length-match-width 0.2
  "Width of vertical bar to display for a zero-length match.
This is relative to the normal width of a character."
  :type 'number)

;;;;; Functions for use in configuration

;; Stolen (with love) from
;; <https://github.com/raxod502/prescient.el/blob/7fd8c3b8028da4733434940c4aac1209281bef58/prescient.el#L242-L288>.
(defun ctrlf-split-fuzzy (input)
  "Split INPUT string into subinputs.
The input is split on spaces, but a sequence of two or more
spaces has one space removed and is treated literally rather than
as a subinput delimiter."
  (if (string-match-p "\\` *\\'" input)
      ;; If string is zero or one spaces, then we match everything.
      ;; Return an empty sub-input list.
      (unless (<= (length input) 1)
        ;; Otherwise, the number of spaces should be reduced by one.
        (list (substring input 1)))
    ;; Trim off a single space from the beginning and end, if present.
    ;; Otherwise, they would generate empty splits and cause us to
    ;; match literal whitespace.
    (setq input (replace-regexp-in-string
                 "\\` ?\\(.*?\\) ?\\'" "\\1" input 'fixedcase))
    (let ((splits (split-string input " "))
          (subinput "")
          (token-found nil)
          (subqueries nil))
      (dolist (split splits)
        ;; Check for empty split, meaning two consecutive spaces in
        ;; the original input.
        (if (string-empty-p split)
            (progn
              ;; Consecutive spaces mean literal spaces in the
              ;; subinput under construction.
              (setq subinput (concat subinput " "))
              ;; If we get a non-empty split, append it to the
              ;; subinput rather than parsing it as another subinput.
              (setq token-found nil))
          ;; Possibly add the collected string as a new subinput.
          (when token-found
            (push subinput subqueries)
            (setq subinput ""))
          ;; Either start a new subinput or append to the existing one
          ;; (in the case of previously seeing an empty split).
          (setq subinput (concat subinput split))
          ;; If another non-empty split is found, it's a separate
          ;; subinput.
          (setq token-found t)))
      ;; Check if we hit the end of the string while still
      ;; constructing a subinput, and handle.
      (unless (string-empty-p subinput)
        (push subinput subqueries))
      ;; We added the subqueries in reverse order.
      (nreverse subqueries))))

(defun ctrlf-translate-fuzzy-literal (input)
  "Build a fuzzy-matching regexp from literal INPUT.
See `ctrlf-split-fuzzy' for how INPUT is split into subinputs.
Each subinput is quoted and the results are joined with \".*\"."
  (string-join (mapcar #'regexp-quote (ctrlf-split-fuzzy input)) ".*"))

(defun ctrlf-translate-fuzzy-regexp (input)
  "Build a fuzzy-matching regexp from regexp INPUT.
See `ctrlf-split-fuzzy' for how INPUT is split into subinputs.
The subinputs are joined with \".*\"."
  (string-join (ctrlf-split-fuzzy input) ".*"))

(defun ctrlf-no-uppercase-literal-p (input)
  "Return non-nil if literal INPUT contains no uppercase letters."
  (isearch-no-upper-case-p input nil))

(defun ctrlf-no-uppercase-regexp-p (input)
  "Return non-nil if regexp INPUT contains no uppercase letters."
  (isearch-no-upper-case-p input t))

;;;;; Faces

(defface ctrlf-highlight-active
  '((t :inherit isearch))
  "Face used to highlight current match.")

(defface ctrlf-highlight-passive
  '((t :inherit lazy-highlight))
  "Face used to highlight other matches in the buffer.")

(defface ctrlf-highlight-line
  '((t :inherit hl-line))
  "Face used to highlight current line.")

;;;;; Variables

(defvar ctrlf-search-history nil
  "History of searches that were not canceled.")

;;;; Session variables
;;;;; Invariant session variables

(defvar ctrlf--active-p nil
  "Non-nil means we're currently performing a search.
This is dynamically bound by CTRLF commands.")

(defvar ctrlf--starting-point nil
  "Value of point from when search was started.")

(defvar ctrlf--minibuffer nil
  "The minibuffer being used for search.")

;;;;; Non-invariant session variables

(defvar ctrlf--style nil
  "Current search style.")

(defvar ctrlf--backward-p nil
  "Non-nil means we are currently searching backward.
Nil means we are currently searching forward.")

(defvar ctrlf--case-fold-search :auto
  "Whether `case-fold-search' is enabled in the current CTRLF session.
Value `:auto' means to guess based on the current search query.")

(defvar ctrlf--current-starting-point nil
  "Value of point from which to search.")

(defvar ctrlf--match-bounds nil
  "Cons cell of current match beginning and end, or nil if no match.")

;;;; Overlay shenanigans

(defvar ctrlf--overlays nil
  "List of all overlays used by CTRLF.
They all have a non-nil `ctrlf' property so that we can identify
them when reading directly from the buffer. Most of our overlays
have an `after-string' property. Some have a `ctrlf--transient'
property which indicates that they should be removed after the
next command. Also, some sub-sections of the string in
`after-string' can have a non-nil `ctrlf--transient' property
which indicates that those sub-sections should be removed after
the next command. (This supports the condensation of persistent
and transient overlays together; see
`ctrlf--condense-overlays'.)")

(defun ctrlf--condense-overlays ()
  "Combine multiple overlays with `after-string' properties into one.
Look at `ctrlf--overlays' to identify groups of overlays that are
at the same buffer position, and merge those.

This function should not be necessary, but there are several
reasons why multiple `after-string' overlays at the same point do
not behave well. One is that in some cases overlay priorities are
not considered correctly when Emacs decides which string to show
first. Another is that multiple `after-string' overlays with
`cursor' properties on their strings will cause the cursor to
render between them rather than at the position indicated by the
highest-priority overlay.

Note that this function is a horrifying hack which cannot
possibly be expected to do the right thing in general, only in
certain special cases within CTRLF and frankly I wouldn't even
trust it in that context."
  (dolist (ols (map-values
                (seq-group-by
                 (lambda (ol)
                   (cons
                    (overlay-buffer ol)
                    (overlay-end ol)))
                 ctrlf--overlays)))
    (setq ols
          (cl-delete-if-not
           (lambda (ol)
             (overlay-get ol 'after-string))
           ols))
    (when ols
      (setq ols
            (cl-sort
             ols #'>
             :key (lambda (ol)
                    (let ((priority (overlay-get ol 'priority)))
                      (if (numberp priority)
                          priority
                        0)))))
      (let ((str
             (mapconcat
              (lambda (ol)
                (let ((str (overlay-get ol 'after-string)))
                  (when (overlay-get ol 'ctrlf--transient)
                    (put-text-property
                     0 (length str) 'ctrlf--transient t str))
                  str))
              ols
              "")))
        (remove-text-properties 0 (length str) 'cursor str)
        (put-text-property 0 1 'cursor t str)
        (overlay-put (car ols) 'after-string str)
        (overlay-put (car ols) 'ctrlf--transient nil)
        (mapc #'delete-overlay (cdr ols)))))
  (setq ctrlf--overlays
        (cl-delete-if-not #'overlay-buffer ctrlf--overlays)))

(defun ctrlf--delete-all-overlays ()
  "Delete all overlays in `ctrlf--overlays'."
  (mapc #'delete-overlay ctrlf--overlays)
  (setq ctrlf--overlays nil))

(defun ctrlf--delete-transient-overlays (&optional negate)
  "Delete overlays marked as transient in `ctrlf--overlays'.
If only part of an overlay is marked as transient (due to
condensation; see `ctrlf--condense-overlays'), only delete that
part. NEGATE non-nil means delete overlays *not* marked as
transient."
  (dolist (ol ctrlf--overlays)
    (if (xor (overlay-get ol 'ctrlf--transient) negate)
        (delete-overlay ol)
      (if-let ((str (overlay-get ol 'after-string)))
          (let ((idx 0))
            (while (< idx (length str))
              (let ((cur (get-text-property idx 'ctrlf--transient str))
                    (next-idx
                     (or (next-property-change idx str)
                         (length str))))
                (if (xor cur negate)
                    (setq str (concat
                               (substring str 0 idx)
                               (substring str next-idx)))
                  (setq idx next-idx))))
            (if (string-empty-p str)
                (delete-overlay ol)
              (remove-text-properties 0 (length str) 'cursor str)
              (put-text-property 0 1 'cursor t str)
              (overlay-put ol 'after-string str))))))
  (setq ctrlf--overlays
        (cl-delete-if-not #'overlay-buffer ctrlf--overlays)))

(defun ctrlf--delete-persistent-overlays (&optional negate)
  "Delete overlays *not* marked as transient in `ctrlf--overlays'.
If only part of an overlay is not marked as transient (due to
condensation; see `ctrlf--condense-overlays'), only delete that
part. NEGATE non-nil means delete overlays that *are* marked as
transient."
  (ctrlf--delete-transient-overlays (not negate)))

(defun ctrlf--minibuffer-message-condense (func &rest args)
  "Apply `ctrlf--fix-overlay-cursor-props' after `minibuffer-message'.

This is an `:around' advice for `minibuffer-message'. FUNC and
ARGS the original function and its arguments, as usual."
  (cl-letf* ((make-overlay (symbol-function #'make-overlay))
             ((symbol-function #'make-overlay)
              (lambda (&rest args)
                (let ((ol (apply make-overlay args)))
                  (prog1 ol
                    ;; Assume ownership of this overlay so we can mess
                    ;; with it :D
                    (overlay-put ol 'ctrlf t)
                    (overlay-put ol 'ctrlf--transient t)
                    (push ol ctrlf--overlays)))))
             (sit-for (symbol-function #'sit-for))
             ((symbol-function #'sit-for)
              (lambda (&rest args)
                ;; Have to stick this inside of `sit-for' because
                ;; `minibuffer-message' uses `sit-for' instead of
                ;; returning.
                (ctrlf--condense-overlays)
                (apply sit-for args))))
    (apply func args)))

(defvar ctrlf--message-persist-p nil
  "Whether `ctrlf--message' will show persistent messages.
If non-nil, then messages will persist until the next
recomputation of CTRLF's overlays. Otherwise, they will only last
until the next interactive command. Persistent messages will be
shown to the left of transient messages.")

(defvar ctrlf--message-in-buffer-p nil
  "Whether `ctrlf--message' will also display its message in the buffer.
By default messages are shown only in the minibuffer.")

(defun ctrlf--message (format &rest args)
  "Display a transient message in the minibuffer.
FORMAT and ARGS are as in `message'. This function behaves
exactly the same as `message' in Emacs 27 and later, and it acts
as a backport for Emacs 26 and earlier where signaling a message
while the minibuffer is active causes an absolutely horrendous
mess."
  ;; Some of this is borrowed from `minibuffer-message'.
  (let ((string (apply #'format (concat " [" format "]") args)))
    (put-text-property 0 (length string) 'face 'minibuffer-prompt string)
    (with-current-buffer ctrlf--minibuffer
      ;; Setting REAR-ADVANCE:
      ;; <https://github.com/raxod502/ctrlf/issues/4>
      (let ((ol (make-overlay (point-max) (point-max) nil nil 'rear-advance)))
        (push ol ctrlf--overlays)
        (overlay-put ol 'ctrlf t)
        (unless ctrlf--message-persist-p
          (overlay-put ol 'ctrlf--transient t))
        (overlay-put
         ol 'priority
         ;; Prioritize our messages over ones generated by Emacs, and
         ;; persistent messages over transient ones.
         (if ctrlf--message-persist-p 2 1))
        (overlay-put ol 'after-string string))
      (when ctrlf--message-in-buffer-p
        (with-current-buffer (window-buffer
                              (minibuffer-selected-window))
          (let* ((loc (point-at-eol))
                 (ol (make-overlay loc loc)))
            (push ol ctrlf--overlays)
            (overlay-put ol 'ctrlf t)
            (unless ctrlf--message-persist-p
              (overlay-put ol 'ctrlf--transient t))
            (overlay-put ol 'priority 1)
            (overlay-put ol 'after-string string))))))
  (ctrlf--condense-overlays))

;;;; Search primitive

(cl-defun ctrlf--search
    (query &key
           (style :unset) (backward :unset) (forward :unset)
           bound)
  "Single-buffer text search primitive. Search for QUERY.
STYLE controls the search style. If it's unset, use the value of
`ctrlf--style'. BACKWARD controls whether to do a forward
search (nil) or a backward search (non-nil), else check
`ctrlf--backward-p'. FORWARD does the same but the meaning of its
argument is inverted. BOUND, if non-nil, is a limit for the
search as in `search-forward' and friends. BOUND can also be the
symbol `wraparound', meaning keep searching at the beginning (or
end, respectively) of the buffer, rather than stopping. If the
search succeeds, move point to the end (for forward searches) or
beginning (for backward searches) of the match. If the search
fails, return nil, but still move point. Otherwise, return
non-nil."
  (let* ((style (cond
                 ((not (eq style :unset))
                  style)
                 (t
                  ctrlf--style)))
         (backward (cond
                    ((not (eq backward :unset))
                     backward)
                    ((not (eq forward :unset))
                     (not forward))
                    (t
                     ctrlf--backward-p)))
         (func (if backward
                   #'re-search-backward
                 #'re-search-forward))
         (query (funcall
                 (plist-get (alist-get style ctrlf-style-alist) :translator)
                 query))
         (wraparound (eq bound 'wraparound))
         (bound (and (integer-or-marker-p bound) bound)))
    (or (funcall func query bound 'noerror)
        (when wraparound
          (goto-char
           (if backward
               (point-max)
             (point-min)))
          (funcall func query nil 'noerror)))))

;;;; Main loop

(defun ctrlf--minibuffer-before-change-function (&rest _)
  "Prepare for user input."
  ;; Clear overlays pre-emptively. See
  ;; <https://github.com/raxod502/ctrlf/issues/1>.
  (ctrlf--delete-transient-overlays))

;;;;; Bookkeeping variables

(defvar ctrlf--last-input nil
  "Previous user input, or nil if none yet.")

(defvar ctrlf--case-fold-search-toggled nil
  "Whether `case-fold-search' has been toggled, so a message should be shown.")

(defvar ctrlf--case-fold-search-last-guessed nil
  "Last guessed value of `case-fold-search'.")

(defvar ctrlf--opened-overlays nil
  "List of overlays that were temporarily made visible to show matches.")

;;;;; Utility functions

(defun ctrlf--copy-properties (s1 s2)
  "Return a copy of S1 with properties from S2 added.
Assume that S2 has the same properties throughout."
  (apply #'propertize s1 (text-properties-at 0 s2)))

(defun ctrlf--prompt ()
  "Return the prompt to use in the minibuffer."
  (concat
   "CTRLF "
   (if ctrlf--backward-p "↑" "↓")
   " "
   (plist-get (alist-get ctrlf--style ctrlf-style-alist) :prompt)
   ": "))

;;;;;; Invisible overlay management

(defun ctrlf--restore-all-invisible-overlays ()
  "Restore any overlays that were previously disabled."
  (while ctrlf--opened-overlays
    (let ((ol (pop ctrlf--opened-overlays)))
      (if-let ((func (overlay-get ol 'isearch-open-invisible-temporary)))
          (funcall func t)
        (overlay-put ol 'invisible (overlay-get ol 'ctrlf-orig-invisible))
        ;; I don't see a function for removing an overlay property, and
        ;; Isearch does it by setting the property to nil, so I assume
        ;; it's fine.
        (overlay-put ol 'ctrlf-orig-invisible nil)))))

(defun ctrlf--disable-invisible-overlays-at-point (&optional permanently)
  "Disable any overlays that are currently hiding point.
PERMANENTLY non-nil means the overlays will not be restored
later (this should be used at the end of the search)."
  (when ctrlf--match-bounds
    (dolist (ol (overlays-in (car ctrlf--match-bounds)
                             (cdr ctrlf--match-bounds)))
      (when (and (invisible-p (overlay-get ol 'invisible))
                 ;; If this function is missing, then we can't open
                 ;; the overlay permanently because we don't know how
                 ;; to do it properly. Hey, don't ask me, I'm just
                 ;; following Isearch.
                 (overlay-get ol 'isearch-open-invisible))
        (if permanently
            (funcall (overlay-get ol 'isearch-open-invisible) ol)
          (push ol ctrlf--opened-overlays)
          (if-let ((func (overlay-get ol 'isearch-open-invisible-temporary)))
              (funcall func nil)
            (overlay-put ol 'ctrlf-orig-invisible (overlay-get ol 'invisible))
            (overlay-put ol 'invisible nil)))))))

;;;;; Post-command hook

(defun ctrlf--minibuffer-post-command-hook ()
  "Deal with updated user input."
  (save-excursion
    (let* ((old-prompt (field-string (point-min)))
           (new-prompt (ctrlf--copy-properties (ctrlf--prompt) old-prompt))
           (inhibit-read-only t)
           ;; Prevent this getting set to t by the editing below.
           (deactivate-mark nil))
      (goto-char (point-min))
      (delete-region (point) (field-end (point)))
      (insert new-prompt)))
  (when (< (point) (field-end (point-min)))
    (goto-char (field-end (point-min))))
  (ctrlf--delete-transient-overlays)
  (cl-block nil
    (let* ((input (field-string (point-max)))
           (translator (plist-get
                        (alist-get ctrlf--style ctrlf-style-alist)
                        :translator))
           (regexp (funcall translator input))
           (case-fold-search
            (if (eq ctrlf--case-fold-search :auto)
                (setq ctrlf--case-fold-search-last-guessed
                      (funcall (plist-get
                                (alist-get
                                 ctrlf--style ctrlf-style-alist)
                                :case-fold)
                               input))
              ctrlf--case-fold-search))
           ;; Simple hack for the sake of performance, because taking
           ;; a regexp that always matches and matching it against the
           ;; entire buffer takes a long time, and we should avoid
           ;; doing this every time CTRLF is launched.
           (skip-search (string-empty-p regexp)))
      (condition-case e
          (string-match-p regexp "")
        (invalid-regexp
         (ctrlf--message "Invalid regexp: %s" (cadr e))
         (setq skip-search t)))
      (unless (equal input ctrlf--last-input)
        (setq ctrlf--last-input input)
        (with-current-buffer (window-buffer (minibuffer-selected-window))
          ;; Jump to the next match.
          (let ((prev-point (point)))
            (goto-char ctrlf--current-starting-point)
            (if (and (not skip-search)
                     (ctrlf--search input :bound 'wraparound))
                (progn
                  (goto-char (match-beginning 0))
                  (setq ctrlf--match-bounds
                        (cons (match-beginning 0)
                              (match-end 0))))
              (goto-char prev-point)
              (setq ctrlf--match-bounds nil)))
          (set-window-point (minibuffer-selected-window) (point))
          (when ctrlf-auto-recenter
            (with-selected-window
                (minibuffer-selected-window)
              (recenter)))
          (ctrlf--delete-persistent-overlays)
          ;; You might think we could do this before clearing
          ;; persistent overlays, because the message overlay in this
          ;; case will be transient. Unfortunately this does not work,
          ;; because overlay condensing may combine the transient
          ;; overlay into a persistent one, which will then get
          ;; deleted before the user can see it.
          (when ctrlf--case-fold-search-toggled
            (ctrlf--message
             "Case-sensitivity %s"
             (if case-fold-search
                 "disabled"
               "enabled"))
            (setq ctrlf--case-fold-search-toggled nil))
          (when ctrlf--match-bounds
            ;; Make sure the match is visible. See:
            ;; <https://github.com/raxod502/ctrlf/issues/23>
            ;; <https://github.com/abo-abo/swiper/blob/64f05f4735bba8b708bc12cfc2cbfb7fb7706787/swiper.el#L878-L885>
            (ctrlf--restore-all-invisible-overlays)
            (ctrlf--disable-invisible-overlays-at-point)
            ;; If there was a match, find all the other matches in the
            ;; buffer. Count them and highlight the ones that appear
            ;; in the window. Display that info in the minibuffer.
            ;;
            ;; You might think we'd want to use `window-start' and
            ;; `window-end' to determine which matches to passively
            ;; highlight. And you'd be right... if those functions
            ;; actually returned correct values. Unfortunately, they
            ;; return the values that *were* correct at the time of
            ;; the last redisplay, which means since we moved point we
            ;; would need to force a redisplay to get the right
            ;; values. Doing that leads to
            ;; <https://github.com/raxod502/ctrlf/issues/18>, so I
            ;; came up with the workaround of just being conservative
            ;; and highlighting a little more than we need, to be sure
            ;; that we get everything necessary, without having to
            ;; highlight the whole buffer which would be very slow.
            (let* ((window-height (window-body-height
                                   (minibuffer-selected-window)))
                   (start (save-excursion
                            (forward-line (- window-height))
                            (point)))
                   (end (save-excursion
                          (forward-line window-height)
                          (point)))
                   (cur-point (point))
                   (num-matches 0)
                   (cur-index nil)
                   (abort nil))
              (save-excursion
                (goto-char (point-min))
                (while (and (not abort)
                            (prog1 (ctrlf--search input :forward t)
                              (when (= (match-beginning 0) (match-end 0))
                                (condition-case _
                                    (forward-char)
                                  (end-of-buffer (setq abort t))))))
                  (when (and (>= (match-end 0) start)
                             (<= (match-beginning 0) end)
                             (or (<= (match-end 0)
                                     (car ctrlf--match-bounds))
                                 (>= (match-beginning 0)
                                     (cdr ctrlf--match-bounds)))
                             ;; You might think we could get away
                             ;; without this, since overlaying the
                             ;; active face below would just
                             ;; overwrite the assignment here. But
                             ;; that doesn't work for zero-length
                             ;; matches.
                             (/= (match-beginning 0)
                                 (car ctrlf--match-bounds)))
                    (let ((ol (make-overlay
                               (match-beginning 0) (match-end 0))))
                      (push ol ctrlf--overlays)
                      (overlay-put ol 'ctrlf t)
                      (overlay-put ol 'priority 2)
                      (if (/= (match-beginning 0) (match-end 0))
                          (overlay-put ol 'face 'ctrlf-highlight-passive)
                        (overlay-put
                         ol 'after-string
                         (propertize
                          " "
                          'display
                          `(space :width ,ctrlf-zero-length-match-width)
                          'face 'ctrlf-highlight-passive)))))
                  (cl-incf num-matches)
                  (when (and (null cur-index)
                             (>= (point) cur-point))
                    (setq cur-index num-matches))))
              (with-current-buffer ctrlf--minibuffer
                (when cur-index
                  (let ((ctrlf--message-persist-p t)
                        (ctrlf--message-in-buffer-p
                         ctrlf-show-match-count-at-eol))
                    (ctrlf--message
                     "%d/%d" cur-index num-matches)))))
            ;; Highlight the active match specially, and optionally also
            ;; the line on which it appears.
            (when ctrlf--match-bounds
              (let ((ol (make-overlay
                         (car ctrlf--match-bounds) (cdr ctrlf--match-bounds))))
                (push ol ctrlf--overlays)
                (overlay-put ol 'ctrlf t)
                (overlay-put ol 'priority 2)
                (if (/= (car ctrlf--match-bounds) (cdr ctrlf--match-bounds))
                    (overlay-put ol 'face 'ctrlf-highlight-active)
                  (overlay-put ol 'ctrlf-skip-highlighting t)
                  (overlay-put
                   ol 'after-string
                   (propertize
                    " "
                    'display
                    `(space :width ,ctrlf-zero-length-match-width)
                    'face 'ctrlf-highlight-active))))
              (when ctrlf-highlight-current-line
                (let* ((start (save-excursion
                                (goto-char (car ctrlf--match-bounds))
                                (line-beginning-position)))
                       (end (save-excursion
                              (goto-char (cdr ctrlf--match-bounds))
                              (line-beginning-position 2)))
                       (ol (make-overlay start end)))
                  (push ol ctrlf--overlays)
                  (overlay-put ol 'ctrlf t)
                  (overlay-put ol 'face 'ctrlf-highlight-line)
                  (dolist (ol (overlays-in start end))
                    (when (and (overlay-get ol 'ctrlf)
                               (not (overlay-get ol 'ctrlf-skip-highlighting)))
                      (when-let ((string (overlay-get ol 'after-string)))
                        ;; No need to worry about removing the face
                        ;; later, as the overlay will get destroyed
                        ;; anyway by the time that this becomes
                        ;; relevant.
                        (add-face-text-property
                         0 (length string)
                         'ctrlf-highlight-line nil string)))))))))))))

;;;; Teardown

(defvar ctrlf--final-window-start nil
  "Original buffer's `window-start' just before exiting minibuffer.
For some reason this gets trashed when exiting the minibuffer, so
we restore it to keep the scroll position consistent.

I have literally no idea why this is needed.")

(defun ctrlf--finalize ()
  "Perform cleanup that has to happen after the minibuffer is exited.
And self-destruct this hook."
  (remove-hook 'post-command-hook #'ctrlf--finalize)
  (unless (= (point) ctrlf--starting-point)
    (push-mark ctrlf--starting-point))
  (set-window-start nil ctrlf--final-window-start))

(defun ctrlf--minibuffer-exit-hook ()
  "Clean up CTRLF from minibuffer and self-destruct this hook."
  (setq ctrlf--final-window-start (window-start (minibuffer-selected-window)))
  (ctrlf--delete-all-overlays)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (ctrlf--restore-all-invisible-overlays)
    (ctrlf--disable-invisible-overlays-at-point 'permanently))
  (remove-hook
   'post-command-hook #'ctrlf--minibuffer-post-command-hook 'local)
  (remove-hook
   'before-change-functions #'ctrlf--minibuffer-before-change-function 'local)
  (remove-hook 'minibuffer-exit-hook #'ctrlf--minibuffer-exit-hook 'local)
  (add-hook 'post-command-hook #'ctrlf--finalize))

;;;; Main entry point

(defun ctrlf--start (&optional arg)
  "Start CTRLF session assuming config vars are set up already.
Use optional ARG as initial contents."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap minibuffer-local-map)
    (map-apply
     (lambda (key cmd)
       (when (stringp key)
         (setq key (kbd key)))
       (define-key keymap key cmd))
     ctrlf-minibuffer-bindings)
    (setq ctrlf--starting-point (point))
    (setq ctrlf--current-starting-point (point))
    (setq ctrlf--last-input nil)
    (setq ctrlf--case-fold-search :auto)
    (setq ctrlf--case-fold-search-toggled nil)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq ctrlf--minibuffer (current-buffer))
          (add-hook
           'minibuffer-exit-hook #'ctrlf--minibuffer-exit-hook nil 'local)
          (add-hook 'post-command-hook #'ctrlf--minibuffer-post-command-hook
                    nil 'local)
          (add-hook 'before-change-functions
                    #'ctrlf--minibuffer-before-change-function
                    nil 'local))
      (let ((ctrlf--active-p t)
            (cursor-in-non-selected-windows nil)
            (blink-matching-paren nil))
        (read-from-minibuffer
         (ctrlf--prompt) arg keymap nil 'ctrlf-search-history
         (thing-at-point 'symbol t))))))

;;;; Public functions
;;;;; Navigation

(defun ctrlf-next-match ()
  "Move to next match, if there is one. Wrap around if necessary."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (when ctrlf--match-bounds
      ;; Move past current match.
      (setq ctrlf--current-starting-point (cdr ctrlf--match-bounds))
      ;; Handle zero-length matches.
      (when (= (car ctrlf--match-bounds)
               (cdr ctrlf--match-bounds))
        (if (/= ctrlf--current-starting-point (point-max))
            (cl-incf ctrlf--current-starting-point)
          (setq ctrlf--current-starting-point (point-min)))))
    ;; Next search should go forward.
    (setq ctrlf--backward-p nil)
    ;; Force recalculation of search.
    (setq ctrlf--last-input nil)))

(defun ctrlf-previous-match ()
  "Move to previous match, if there is one. Wrap around if necessary."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (when ctrlf--match-bounds
      ;; Move before current match.
      (setq ctrlf--current-starting-point (car ctrlf--match-bounds))
      ;; Handle zero-length matches.
      (when (= (car ctrlf--match-bounds)
               (cdr ctrlf--match-bounds))
        (if (/= ctrlf--current-starting-point (point-min))
            (cl-decf ctrlf--current-starting-point)
          (setq ctrlf--current-starting-point (point-max))))))
  ;; Next search should go backward.
  (setq ctrlf--backward-p t)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-first-match ()
  "Move to first match, if there is one."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (when ctrlf--match-bounds
      (setq ctrlf--current-starting-point (point-min))))
  ;; Next search should go forward.
  (setq ctrlf--backward-p nil)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-last-match ()
  "Move to last match, if there is one."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (when ctrlf--match-bounds
      (setq ctrlf--current-starting-point (point-max))))
  ;; Next search should go backward.
  (setq ctrlf--backward-p t)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-next-page ()
  "Move to first match appearing after the current screen, if there is one.
Wrap around if necessary."
  (interactive)
  (when ctrlf--match-bounds
    (setq ctrlf--current-starting-point
          (window-end (minibuffer-selected-window))))
  ;; Next search should go forward.
  (setq ctrlf--backward-p nil)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-previous-page ()
  "Move to first match appearing before the current screen, if there is one.
Wrap around if necessary."
  (interactive)
  (when ctrlf--match-bounds
    (setq ctrlf--current-starting-point
          (window-start (minibuffer-selected-window))))
  ;; Next search should go backward.
  (setq ctrlf--backward-p t)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

;;;;; Main commands

(defun ctrlf-forward (style &optional preserve arg)
  "Search forward using given STYLE (see `ctrlf-style-alist').
If already in a search, go to next candidate, or if no input then
insert the previous search string. PRESERVE non-nil means don't
change the search style if already in a search. Start (or continue)
the search with ARG."
  (unless ctrlf--active-p
    (setq preserve nil))
  (let ((inhibit-history (or (and (not preserve)
                                  (not (eq style ctrlf--style)))
                             (not (eq nil   ctrlf--backward-p)))))
    (unless preserve
      (setq ctrlf--style style))
    (if ctrlf--active-p
        (cond
         ;; Continue search with `arg'.
         ;; TODO: Is there a better way to replace the minibuffer contents?
         (arg
          (delete-minibuffer-contents)
          (insert arg))
         ;; Insert the previous search string.
         ((and (not inhibit-history)
               (string-empty-p (field-string (point-max))))
          (previous-history-element 1))
         ;; Go to next candidate.
         (t (ctrlf-next-match)))
      (setq ctrlf--backward-p nil)
      (ctrlf--start arg))))

(defun ctrlf-backward (style &optional preserve)
  "Search backward using given STYLE (see `ctrlf-style-alist').
If already in a search, go to previous candidate, or if no input
then insert the previous search string. PRESERVE non-nil means
don't change the search style if already in a search."
  (unless ctrlf--active-p
    (setq preserve nil))
  (let ((inhibit-history (or (and (not preserve)
                                  (not (eq style ctrlf--style)))
                             (not      (eq t     ctrlf--backward-p)))))
    (unless preserve
      (setq ctrlf--style style))
    (if ctrlf--active-p
        (if (and (not inhibit-history)
                 (string-empty-p (field-string (point-max))))
            (previous-history-element 1)
          (ctrlf-previous-match))
      (setq ctrlf--backward-p t)
      (ctrlf--start))))

;;;;; Utilities

(defun ctrlf-change-search-style (style)
  "Switch to given search STYLE for current CTRLF session.
Interactively, select from the list of all defined styles."
  (interactive
   (list
    (intern
     (let ((enable-recursive-minibuffers t))
       (completing-read
        "Style: "
        (mapcar #'symbol-name (map-keys ctrlf-style-alist)))))))
  (setq ctrlf--style style)
  (setq ctrlf--last-input nil))

(defun ctrlf-toggle-case-fold-search ()
  "Toggle `case-fold-search' for current CTRLF session.
By default the appropriate value is guessed by checking whether
the input has any uppercase letters. Toggling will fix the value,
initially to the opposite of the guessed value for the current
query."
  (interactive)
  (when (eq ctrlf--case-fold-search :auto)
    (setq ctrlf--case-fold-search
          ctrlf--case-fold-search-last-guessed))
  (setq ctrlf--case-fold-search
        (not ctrlf--case-fold-search))
  ;; Queue a message to be displayed.
  (setq ctrlf--case-fold-search-toggled t)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-recenter-top-bottom ()
  "Display current match in the center of the window (by default).
Successive calls, or calls with prefix argument, may have
different behavior, for which see `recenter-top-bottom'."
  (interactive)
  (with-selected-window
      (minibuffer-selected-window)
    (recenter-top-bottom)))

;;;;; Exit

(defun ctrlf-cancel ()
  "Exit search, returning point to original position."
  (interactive)
  (ctrlf--delete-all-overlays)
  (set-window-point (minibuffer-selected-window) ctrlf--starting-point)
  ;; Dirty hack to solve <https://github.com/raxod502/ctrlf/issues/6>.
  (redisplay)
  (abort-recursive-edit))

;;;;; Main-command wrappers for built-in styles

;;;###autoload
(defun ctrlf-forward-literal (&optional arg)
  "Search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-literal search,
change back to literal search if prefix ARG is provided. If in
the minibuffer but not in a search already, run command
`isearch-forward' instead."
  (interactive "P")
  (if (and (window-minibuffer-p) (not ctrlf--active-p))
      (isearch-forward)
    (ctrlf-forward 'literal (null arg))))

;;;###autoload
(defun ctrlf-backward-literal (&optional arg)
  "Search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-literal
search, change back to literal search if prefix ARG is provided.
If in the minibuffer but not in a search already, run
`isearch-backward' instead."
  (interactive "P")
  (if (and (window-minibuffer-p) (not ctrlf--active-p))
      (isearch-backward)
    (ctrlf-backward 'literal (null arg))))

;;;###autoload
(defun ctrlf-forward-regexp ()
  "Search forward for regexp.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-regexp search,
change back to regexp search."
  (interactive)
  (if (and (window-minibuffer-p) (not ctrlf--active-p))
      (isearch-forward-regexp)
    (ctrlf-forward 'regexp)))

;;;###autoload
(defun ctrlf-backward-regexp ()
  "Search backward for regexp.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-regexp
search, change back to regexp search."
  (interactive)
  (if (and (window-minibuffer-p) (not ctrlf--active-p))
      (isearch-backward-regexp)
    (ctrlf-backward 'regexp)))

(defun ctrlf-forward-fuzzy ()
  "Fuzzy search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-fuzzy search,
change back to fuzzy search."
  (interactive)
  (ctrlf-forward 'fuzzy))

(defun ctrlf-backward-fuzzy ()
  "Fuzzy search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-fuzzy search,
change back to fuzzy search."
  (interactive)
  (ctrlf-backward 'fuzzy))

(defun ctrlf-forward-fuzzy-regexp ()
  "Fuzzy search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-fuzzy-regexp
search, change back to fuzzy-regexp search."
  (interactive)
  (ctrlf-forward 'fuzzy-regexp))

(defun ctrlf-backward-fuzzy-regexp ()
  "Fuzzy search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-fuzzy-regexp
search, change back to fuzzy-regexp search."
  (interactive)
  (ctrlf-backward 'fuzzy-regexp))

;;;; Minor mode

;;;###autoload
(defvar ctrlf--keymap (make-sparse-keymap)
  "Keymap for `ctrlf-mode'. Populated when mode is enabled.
See `ctrlf-mode-bindings'.")

;;;###autoload
(progn
  (define-minor-mode ctrlf-mode
    "Minor mode to use CTRLF in place of Isearch.
See `ctrlf-mode-bindings' to customize."
    :global t
    :keymap ctrlf--keymap
    (require 'map)
    (when ctrlf-mode
      ;; Hack to clear out keymap. Presumably there's a `clear-keymap'
      ;; function lying around somewhere...?
      (setcdr ctrlf--keymap nil)
      (map-apply
       (lambda (key cmd)
         (when (stringp key)
           (setq key (kbd key)))
         (define-key ctrlf--keymap key cmd))
       ctrlf-mode-bindings))
    (with-eval-after-load 'ctrlf
      (if ctrlf-mode
          (advice-add #'minibuffer-message :around
                      #'ctrlf--minibuffer-message-condense)
        (advice-remove #'minibuffer-message
                       #'ctrlf--minibuffer-message-condense)))))

;;;; Closing remarks

(provide 'ctrlf)

;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; sentence-end-double-space: nil
;; End:

;;; ctrlf.el ends here
