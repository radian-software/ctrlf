;;; ctrlf.el --- Emacs finally learns how to ctrl+F -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 23 Dec 2019
;; Homepage: https://github.com/raxod502/ctrlf
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; Version: 0

;;; Commentary:

;; Please see https://github.com/raxod502/ctrlf for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

(require 'cl-lib)
(require 'map)

(defgroup ctrlf nil
  "More streamlined replacement for Isearch, Swiper, etc."
  :group 'convenience
  :prefix "ctrlf-"
  :link '(url-link "https://github.com/raxod502/ctrlf"))

(defcustom ctrlf-mode-bindings
  '(([remap isearch-forward        ] . ctrlf-forward)
    ([remap isearch-backward       ] . ctrlf-backward)
    ([remap isearch-forward-regexp ] . ctrlf-forward-regexp)
    ([remap isearch-backward-regexp] . ctrlf-backward-regexp))
  "Keybindings enabled in `ctrlf-mode'. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
`ctrlf-mode' is (re-)enabled. The keys are strings or raw key
events and the values are command symbols."
  :type '(alist
          :key-type sexp
          :value-type function))

(defcustom ctrlf-minibuffer-bindings
  '(([remap abort-recursive-edit]     . ctrlf-cancel)
    ;; This is bound in `minibuffer-local-map' by loading `delsel', so
    ;; we have to account for it too.
    ([remap minibuffer-keyboard-quit]       . ctrlf-cancel)
    ([remap minibuffer-beginning-of-buffer] . ctrlf-first-match)
    ([remap end-of-buffer]                  . ctrlf-last-match)
    ("C-s"       . ctrlf-next-match-or-previous-history-element)
    ("TAB"       . ctrlf-next-match-or-previous-history-element)
    ("C-r"       . ctrlf-previous-match-or-previous-history-element)
    ("S-TAB"     . ctrlf-previous-match-or-previous-history-element)
    ("<backtab>" . ctrlf-previous-match-or-previous-history-element))
  "Keybindings enabled in minibuffer during search. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
entering the minibuffer. The keys are strings or raw key events
and the values are command symbols. The keymap so constructed
inherits from `minibuffer-local-map'."
  :type '(alist
          :key-type sexp
          :value-type function))

(defface ctrlf-highlight-active
  '((t :inherit isearch))
  "Face used to highlight current match.")

(defface ctrlf-highlight-passive
  '((t :inherit lazy-highlight))
  "Face used to highlight other matches in the buffer.")

(defvar ctrlf-search-history nil
  "History of searches that were not canceled.")

(defvar ctrlf--active-p nil
  "Non-nil means we're currently performing a search.
This is dynamically bound by CTRLF commands.")

(defvar ctrlf--backward-p nil
  "Non-nil means we are currently searching backward.
Nil means we are currently searching forward.")

(defvar ctrlf--regexp-p nil
  "Non-nil means we are searching using a regexp.
Nil means we are searching using a literal string.")

(defun ctrlf--minibuffer-exit-hook ()
  "Clean up CTRLF from buffer and minibuffer."
  ;; There's no need to clean up the minibuffer-local hooks as they
  ;; appear to be trashed automatically.
  (ctrlf--clear-highlight-overlays))

(defvar ctrlf--last-input nil
  "Previous user input, or nil if none yet.")

(defvar ctrlf--starting-point nil
  "Value of point from when search was started.")

(defvar ctrlf--current-starting-point nil
  "Value of point from which to search.")

(defvar ctrlf--minibuffer nil
  "The minibuffer being used for search.")

(defvar ctrlf--message-overlay nil
  "Overlay used to display transient message, or nil.")

(defvar ctrlf--highlight-overlays nil
  "List of active overlays used for highlighting.")

(defvar ctrlf--match-bounds nil
  "Cons cell of current match beginning and end, or nil if no match.")

(defun ctrlf--transient-message (format &rest args)
  "Display a transient message in the minibuffer.
FORMAT and ARGS are as in `message'. This function behaves
exactly the same as `message' in Emacs 27 and later, and it acts
as a backport for Emacs 26 and earlier where signaling a message
while the minibuffer is active causes an absolutely horrendous
mess."
  (with-current-buffer ctrlf--minibuffer
    (setq ctrlf--message-overlay (make-overlay (point-max) (point-max)))
    ;; Some of this is borrowed from `minibuffer-message'.
    (let ((string (apply #'format (concat " [" format "]") args)))
      (put-text-property 0 (length string) 'face 'minibuffer-prompt string)
      (put-text-property 0 1 'cursor t string)
      (overlay-put ctrlf--message-overlay 'after-string string))))

(cl-defun ctrlf--search
    (query &key
           (regexp :unset) (backward :unset)
           (literal :unset) (forward :unset)
           wraparound bound)
  "Single-buffer text search primitive. Search for QUERY.
REGEXP controls whether to interpret QUERY literally (nil) or as
a regexp (non-nil), else check `ctrlf--regexp-p'. BACKWARD
controls whether to do a forward search (nil) or a backward
search (non-nil), else check `ctrlf--backward-p'. LITERAL and
FORWARD do the same but the meaning of their arguments are
inverted. BOUND, if non-nil, is a limit for the search as in
`search-forward' and friend. If the search succeeds, move point
to the end (for forward searches) or beginning (for backward
searches) of the match. If the search fails, return nil, but
still move point."
  (let* ((regexp (cond
                  ((not (eq regexp :unset))
                   regexp)
                  ((not (eq literal :unset))
                   (not literal))
                  (t
                   ctrlf--regexp-p)))
         (backward (cond
                    ((not (eq backward :unset))
                     backward)
                    ((not (eq forward :unset))
                     (not forward))
                    (t
                     ctrlf--backward-p)))
         (func (if backward
                   (if regexp
                       #'re-search-backward
                     #'search-backward)
                 (if regexp
                     #'re-search-forward
                   #'search-forward))))
    (or (funcall func query bound 'noerror)
        (when wraparound
          (goto-char
           (if backward
               (point-max)
             (point-min)))
          (funcall func query bound 'noerror)))))

(defun ctrlf--clear-highlight-overlays ()
  "Delete all overlays used for highlighting."
  (while ctrlf--highlight-overlays
    (delete-overlay (pop ctrlf--highlight-overlays))))

(defun ctrlf--minibuffer-post-command-hook ()
  "Deal with updated user input."
  (when ctrlf--message-overlay
    (delete-overlay ctrlf--message-overlay)
    (setq ctrlf--message-overlay nil))
  (cl-block nil
    (let ((input (field-string (point-max))))
      (when ctrlf--regexp-p
        (condition-case e
            (string-match-p input "")
          (invalid-regexp
           (ctrlf--transient-message "Invalid regexp: %s" (cadr e))
           (cl-return))))
      (unless (equal input ctrlf--last-input)
        (setq ctrlf--last-input input)
        (setq ctrlf--match-bounds nil)
        (ctrlf--clear-highlight-overlays)
        (with-current-buffer (window-buffer (minibuffer-selected-window))
          (let ((prev-point (point)))
            (goto-char ctrlf--current-starting-point)
            (if (ctrlf--search input :wraparound t)
                (progn
                  (goto-char (match-beginning 0))
                  (setq ctrlf--match-bounds
                        (cons (match-beginning 0)
                              (match-end 0)))
                  (let ((ol (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put ol 'face 'ctrlf-highlight-active)
                    (push ol ctrlf--highlight-overlays)))
              (goto-char prev-point)))
          (set-window-point (minibuffer-selected-window) (point))
          ;; Apparently `window-end' takes an UPDATE argument but
          ;; `window-start' doesn't? Okay then.
          (let ((start (window-start (minibuffer-selected-window)))
                (end (window-end (minibuffer-selected-window) 'update)))
            (save-excursion
              (goto-char start)
              (while (and (< (point) end)
                          (ctrlf--search input :forward t :bound end))
                (if (= (match-beginning 0) (match-end 0))
                    ;; Zero-length match, no need to highlight. Advance
                    ;; point to avoid infinite loop.
                    (ignore-errors
                      (forward-char))
                  (when (or (null ctrlf--match-bounds)
                            (<= (match-end 0)
                                (car ctrlf--match-bounds))
                            (>= (match-beginning 0)
                                (cdr ctrlf--match-bounds)))
                    (let ((ol (make-overlay
                               (match-beginning 0) (match-end 0))))
                      (overlay-put ol 'face 'ctrlf-highlight-passive)
                      (push ol ctrlf--highlight-overlays))))))))))))

(defun ctrlf--start ()
  "Start CTRLF session assuming config vars are set up already."
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
    (minibuffer-with-setup-hook
        (lambda ()
          (setq ctrlf--minibuffer (current-buffer))
          (add-hook
           'minibuffer-exit-hook #'ctrlf--minibuffer-exit-hook nil 'local)
          (add-hook 'post-command-hook #'ctrlf--minibuffer-post-command-hook
                    nil 'local))
      (let ((ctrlf--active-p t)
            (cursor-in-non-selected-windows
             ;; This conditional is needed because `cursor-type' t
             ;; means to look up the frame parameter, but
             ;; `cursor-in-non-selected-windows' means use a modified
             ;; cursor type derived from `cursor-type' (which behavior
             ;; we are explicitly trying to disable here).
             (if (eq cursor-type t)
                 (frame-parameter nil 'cursor-type)
               cursor-type)))
        (read-from-minibuffer
         "Find: " nil keymap nil 'ctrlf-search-history)))))

(defun ctrlf-forward ()
  "Search forward for literal string."
  (interactive)
  (when ctrlf--active-p
    (user-error "Already in the middle of a CTRLF search"))
  (setq ctrlf--backward-p nil)
  (setq ctrlf--regexp-p nil)
  (ctrlf--start))

(defun ctrlf-backward ()
  "Search backward for literal string."
  (interactive)
  (when ctrlf--active-p
    (user-error "Already in the middle of a CTRLF search"))
  (setq ctrlf--backward-p t)
  (setq ctrlf--regexp-p nil)
  (ctrlf--start))

(defun ctrlf-forward-regexp ()
  "Search forward for regexp."
  (interactive)
  (when ctrlf--active-p
    (user-error "Already in the middle of a CTRLF search"))
  (setq ctrlf--backward-p nil)
  (setq ctrlf--regexp-p t)
  (ctrlf--start))

(defun ctrlf-backward-regexp ()
  "Search backward for regexp."
  (interactive)
  (when ctrlf--active-p
    (user-error "Already in the middle of a CTRLF search"))
  (setq ctrlf--backward-p t)
  (setq ctrlf--regexp-p t)
  (ctrlf--start))

(defun ctrlf-next-match ()
  "Move to next match, if there is one. Wrap around if necessary."
  (interactive)
  (when ctrlf--match-bounds
    ;; Move past current match.
    (setq ctrlf--current-starting-point (cdr ctrlf--match-bounds)))
  ;; Next search should go forward.
  (setq ctrlf--backward-p nil)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-previous-match ()
  "Move to previous match, if there is one. Wrap around if necessary."
  (interactive)
  (when ctrlf--match-bounds
    ;; Move before current match.
    (setq ctrlf--current-starting-point (car ctrlf--match-bounds)))
  ;; Next search should go backward.
  (setq ctrlf--backward-p t)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-next-match-or-previous-history-element ()
  "Move to next match or re-start last search.
Re-start the last search if there is currently no input, and move
to next match otherwise."
  (interactive)
  (if (string-empty-p (field-string (point-max)))
      (previous-history-element 1)
    (ctrlf-next-match)))

(defun ctrlf-previous-match-or-previous-history-element ()
  "Move to previous match or re-start last search.
Re-start the last search if there is currently no input, and move
to previous match otherwise."
  (interactive)
  (if (string-empty-p (field-string (point-max)))
      (previous-history-element 1)
    (ctrlf-previous-match)))

(defun ctrlf-first-match ()
  "Move to first match, if there is one."
  (interactive)
  (when ctrlf--match-bounds
    (setq ctrlf--current-starting-point
          (with-current-buffer
              (window-buffer (minibuffer-selected-window))
            (point-min))))
  ;; Next search should go forward.
  (setq ctrlf--backward-p nil)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-last-match ()
  "Move to last match, if there is one."
  (interactive)
  (when ctrlf--match-bounds
    (setq ctrlf--current-starting-point
          (with-current-buffer
              (window-buffer (minibuffer-selected-window))
            (point-max))))
  ;; Next search should go backward.
  (setq ctrlf--backward-p t)
  ;; Force recalculation of search.
  (setq ctrlf--last-input nil))

(defun ctrlf-cancel ()
  "Exit search, returning point to original position."
  (interactive)
  (ctrlf--clear-highlight-overlays)
  (set-window-point (minibuffer-selected-window) ctrlf--starting-point)
  (abort-recursive-edit))

(defvar ctrlf--keymap (make-sparse-keymap)
  "Keymap for `ctrlf-mode'. Populated when mode is enabled.
See `ctrlf-mode-bindings'.")

(define-minor-mode ctrlf-mode
  "Minor mode to use CTRLF in place of Isearch.
See `ctrlf-mode-bindings' to customize."
  :global t
  :keymap ctrlf--keymap
  (when ctrlf-mode
    (ctrlf-mode -1)
    (setq ctrlf-mode t)
    ;; Hack to clear out keymap. Presumably there's a `clear-keymap'
    ;; function lying around somewhere...?
    (setcdr ctrlf--keymap nil)
    (map-apply
     (lambda (key cmd)
       (when (stringp key)
         (setq key (kbd key)))
       (define-key ctrlf--keymap key cmd))
     ctrlf-mode-bindings)))

;;;; Closing remarks

(provide 'ctrlf)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; ctrlf.el ends here
