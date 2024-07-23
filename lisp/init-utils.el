;;; init-utils.el -- Useful Lisp routines. -*- lexical-binding: t -*-
;;; Commentary:

;; All kinds of small but useful Lisp routines that are copied from everywhere,
;; or written by myself.

;;; Code:


;;; My functions.

(defun celeste/open-init-file ()
  "Open Celeste init file."
  (interactive)
  (find-file celeste-init-file))

(defun pure-save-buffer ()
  "Save current file w/o running `before-save-hook'."
  (interactive)
  (let (before-save-hook after-save-hook write-file-functions)
    (save-buffer)))

(defun reload-this-file ()
  "Reload current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (progn (when (y-or-n-p (format "Reload %s?" file))
                 (kill-current-buffer)
                 (find-file file)))
      (message "The current buffer has no corresponding file!"))))

(defun +add-no-byte-compile-file-local-variable ()
  "Add file-local variable: `no-byte-compile': t."
  (interactive)
  (delete-file-local-variable-prop-line 'no-byte-compile)
  (add-file-local-variable 'no-byte-compile t))

;; Logic copied from `flycheck-this-emacs-executable'.
(defvar emacs-executable-path
  (concat invocation-directory invocation-name)
  "The path to the Emacs executable.")

(defun spawn-sub-emacs (arg)
  "Spawn a sub-Emacs. Mainly for testing purpose.

If ARG is non-nil, spawn a vanilla Emacs."
  (interactive "P")
  (if-let ((emacs-exe (or emacs-executable-path
                          (executable-find "emacs"))))
      (if arg
          (async-shell-command (concat emacs-exe " -Q"))
        (async-shell-command emacs-exe))
    (user-error "Cannot find `emacs' executable!")))

(defun upcase-previous-word ()
  "Upcase the previous word.

Obviously more useful than `upcase-word' and `upcase-dwim', which
only upcase the next word."
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end) (region-noncontiguous-p))
    (save-excursion
      (let* ((cur-pos (point))
             (delim (ignore-errors (if current-prefix-arg
                                       (search-backward-regexp "[[:blank:]\n]")
                                     (search-backward-regexp "[^a-zA-Z0-9]"))))
             (delim (or delim (1- (point-min)))))
        (funcall #'upcase-region (1+ delim) cur-pos)))))

(defun kill-current-file-name ()
  "Kill the current file name, if it visits a real file."
  (interactive)
  (if-let (file-path (buffer-file-name))
      (progn
        (setq file-path (abbreviate-file-name file-path))
        (kill-new file-path)
        (message "Kill \"%s\"." file-path))
    (message "Not a file.")))


(defun keyboard-escape-quit-no-close-win ()
  "Similar to `keyboard-escape-quit', except that no `delete-other-windows' switch."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((region-active-p)
	 (deactivate-mark))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))
    ;; Do not touch my window configuration!
	;; ((not (one-window-p t))
	;;  (delete-other-windows))
	((string-match "^ \\*" (buffer-name (current-buffer)))
	 (bury-buffer))))

(defun eol-dos-to-unix ()
  "Convert from doc-style EOL (CRLF, ^M^J) to unix-style (LF, ^J)."
  (interactive)
  (replace-string-in-region "\n" "\n" (point-min) (point-max)))

(defun eol-unix-to-dos ()
  "Convert from unix-style (LF, ^J) to doc-style EOL (CRLF, ^M^J)."
  (interactive)
  (replace-string-in-region "\n" "\n" (point-min) (point-max)))



;;; Got these idea from bbatsov/crux.

(defun other-window-or-switch-buffer ()
  "Call `other-window' if more than one window is visible.
Switch to most recent buffer otherwise."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer nil)
    (other-window 1)))

(defun kill-buffer-truename ()
  "Kill absolute path of file visited in current buffer."
  (interactive)
  (if buffer-file-name
      (let ((truename (file-truename buffer-file-name)))
        (kill-new truename)
        (message "Added %s to kill ring." truename))
    (message "Buffer is not visiting a file.")))



;;; Delete current file.
;; From doom, simplified.

(defun delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          (kill-buffer buf))))))


;;; Protesilaos

(define-minor-mode hidden-mode-line-mode
  "Toggle modeline visibility in the current buffer."
  :init-value nil
  :global nil
  (if hidden-mode-line-mode
      (setq-local mode-line-format nil)
    (kill-local-variable 'mode-line-format)
    (force-mode-line-update)))

(defun keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))



;;; unfill-region
;; https://www.emacswiki.org/emacs/UnfillRegion

(defun unfill-region (beg end)
  "Unfill the region betweeen BEG and END.

Joining text paragraphs into a single logical line. This is
useful, e.g., for use with `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


;;; half-screen scrolling
;; https://karthinks.com/software/more-less-emacs/#bonus-half-screen-scrolling

(defun scroll-up-half ()
  "Scroll up half screen."
  (interactive)
  (scroll-up-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(defun scroll-down-half ()
  "Scroll down half screen."
  (interactive)
  (scroll-down-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))



;; Key bindings.

(keymap-global-set "s-r" #'reload-this-file)
(keymap-global-set "<f9>" #'spawn-sub-emacs)
(keymap-global-set "C-g" #'keyboard-quit-dwim)
(keymap-global-set "M-u" #'upcase-previous-word)
(keymap-global-set "ESC ESC ESC" #'keyboard-escape-quit-no-close-win)


(provide 'init-utils)
;;; init-utils.el ends here
