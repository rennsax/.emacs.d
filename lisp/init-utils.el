;;; init-utils.el -- Useful Lisp routines. -*- lexical-binding: t -*-
;;; Commentary:

;; All kinds of small but useful Lisp routines that are copied from everywhere,
;; or written by myself.

;;; Code:


;;; My functions.

(defun pure-save-buffer ()
  "Save current file w/o running `before-save-hook'."
  (interactive)
  (let (before-save-hook)
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

(defun byte-compile-this-file ()
  "Byte compile current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file (string-suffix-p "el" file))
        (byte-compile-file file)
      (message "Cannot compile current \"file\"!"))))

(defun +add-no-byte-compile-file-local-variable ()
  "Add file-local variable: `no-byte-compile': t."
  (interactive)
  (delete-file-local-variable-prop-line 'no-byte-compile)
  (add-file-local-variable 'no-byte-compile t))

(defun spawn-sub-emacs ()
  "Spawn a sub-Emacs. Mainly for testing purpose."
  (interactive)
  (async-shell-command "emacs"))



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

(defun keyboard-quit-dwim (arg)
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive "P")
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


;;; Interactively resize window.
;; https://www.emacswiki.org/emacs/WindowResize

;; TODO: rewrite a version like `text-scale-mode'
(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "Resize window interactively in the unit of ARG."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	(message
	 "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
	 arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?h) (enlarge-window arg))
	     ((= c ?s) (shrink-window arg))
	     ((= c ?w) (enlarge-window-horizontally arg))
	     ((= c ?n) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?q) (throw 'done t))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))



;; Key bindings.

(keymap-global-set "s-r" #'reload-this-file)
(keymap-global-set "<f9>" #'spawn-sub-emacs)
(keymap-global-set "s-o" #'other-window-or-switch-buffer)
(keymap-global-set "C-g" #'keyboard-quit-dwim)


(provide 'init-utils)
;;; init-utils.el ends here
