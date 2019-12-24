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

(require 'map)

(defgroup ctrlf nil
  "More streamlined replacement for Isearch, Swiper, etc."
  :group 'convenience
  :prefix "ctrlf-"
  :link '(url-link "https://github.com/raxod502/ctrlf"))

(defcustom ctrlf-mode-bindings
  '(([remap isearch-forward] . ctrlf-forward)
    ([remap isearch-backward] . ctrlf-backward)
    ([remap isearch-forward-regexp] . ctrlf-forward-regexp)
    ([remap isearch-backward-regexp] . ctrlf-backward-regexp))
  "Keybindings enabled in `ctrlf-mode'. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
`ctrlf-mode' is (re-)enabled. The keys are strings or raw key
events and the values are command symbols."
  :type '(alist
          :key-type sexp
          :value-type function))

(defcustom ctrlf-minibuffer-bindings
  '(("C-g" . ctrlf-cancel))
  "Keybindings enabled in minibuffer during search. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
entering the minibuffer. The keys are strings or raw key events
and the values are command symbols."
  :type '(alist
          :key-type sexp
          :value-type function))

(defvar ctrlf--backwardp nil
  "Non-nil means we are currently searching backward.
Nil means we are currently searching forward.")

(defvar ctrlf--regexpp nil
  "Non-nil means we are searching using a regexp.
Nil means we are searching using a literal string.")

(defun ctrlf--minibuffer-exit-hook ()
  "Clean up CTRLF from the minibuffer, and self-destruct this hook."
  (remove-hook
   'post-command-hook #'ctrlf--minibuffer-post-command-hook 'local)
  (remove-hook 'minibuffer-exit-hook #'ctrlf--minibuffer-exit-hook 'local))

(defvar ctrlf--last-input nil
  "Previous user input, or nil if none yet.")

(defvar ctrlf--starting-point nil
  "Value of point from when search was started.")

(defvar ctrlf--minibuffer nil
  "The minibuffer being used for search.")

(defvar ctrlf--message-overlay nil
  "Overlay used to display transient message, or nil.")

(defun ctrlf--transient-message (format &rest args)
  "Display a transient message in the minibuffer."
  (with-current-buffer ctrlf--minibuffer
    (setq ctrlf--message-overlay (make-overlay (point-max) (point-max)))
    ;; Some of this is borrowed from `minibuffer-message'.
    (let ((string (apply #'format (concat " [" format "]") args)))
      (put-text-property 0 (length string) 'face 'minibuffer-prompt string)
      (put-text-property 0 1 'cursor t string)
      (overlay-put ctrlf--message-overlay 'after-string string))))

(defun ctrlf--minibuffer-post-command-hook ()
  "Deal with updated user input."
  (when ctrlf--message-overlay
    (delete-overlay ctrlf--message-overlay)
    (setq ctrlf--message-overlay nil))
  (cl-block nil
    (let ((input (field-string (point-max))))
      (when ctrlf--regexpp
        (condition-case e
            (string-match-p input "")
          (invalid-regexp
           (ctrlf--transient-message "Invalid regexp: %s" (cadr e))
           (cl-return))))
      (unless (equal input ctrlf--last-input)
        (setq ctrlf--last-input input)
        (with-current-buffer (window-buffer (minibuffer-selected-window))
          (let ((prev-point (point)))
            (goto-char ctrlf--starting-point)
            (unless (funcall
                     (if ctrlf--backwardp
                         (if ctrlf--regexpp
                             #'re-search-backward
                           #'search-backward)
                       (if ctrlf--regexpp
                           #'re-search-forward
                         #'search-forward))
                     input nil 'noerror)
              (goto-char prev-point))
            (set-window-point (minibuffer-selected-window) (point))))))))

(defun ctrlf--start ()
  "Start CTRLF session assuming config vars are set up already."
  (let ((keymap (make-sparse-keymap)))
    (map-apply
     (lambda (key cmd)
       (when (stringp key)
         (setq key (kbd key)))
       (define-key keymap key cmd))
     ctrlf-minibuffer-bindings)
    (setq ctrlf--starting-point (point))
    (setq ctrlf--last-input nil)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq ctrlf--minibuffer (current-buffer))
          (add-hook
           'minibuffer-exit-hook #'ctrlf--minibuffer-exit-hook nil 'local)
          (add-hook 'post-command-hook #'ctrlf--minibuffer-post-command-hook
                    nil 'local))
      (read-from-minibuffer "Find: " nil keymap))))

(defun ctrlf-forward ()
  "Search forward for literal string."
  (interactive)
  (setq ctrlf--backwardp nil)
  (setq ctrlf--regexpp nil)
  (ctrlf--start))

(defun ctrlf-backward ()
  "Search backward for literal string."
  (interactive)
  (setq ctrlf--backwardp t)
  (setq ctrlf--regexpp nil)
  (ctrlf--start))

(defun ctrlf-forward-regexp ()
  "Search forward for regexp."
  (interactive)
  (setq ctrlf--backwardp nil)
  (setq ctrlf--regexpp t)
  (ctrlf--start))

(defun ctrlf-backward-regexp ()
  "Search backward for regexp."
  (interactive)
  (setq ctrlf--backwardp t)
  (setq ctrlf--regexpp t)
  (ctrlf--start))

(defun ctrlf-cancel ()
  "Exit search, returning point to original position."
  (interactive)
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
  (if ctrlf-mode
      (progn
        (ctrlf-mode -1)
        (setq ctrlf-mode t)
        ;; Hack to clear out keymap. Presumably there's a
        ;; `clear-keymap' function lying around somewhere...?
        (setcdr ctrlf--keymap nil)
        (map-apply
         (lambda (key cmd)
           (when (stringp key)
             (setq key (kbd key)))
           (define-key ctrlf--keymap key cmd))
         ctrlf-mode-bindings))))

;;;; Closing remarks

(provide 'ctrlf)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; ctrlf.el ends here
