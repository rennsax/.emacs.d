;;; init-func.el -- Useful interactive functions. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(defun celeste/open-init-file ()
  "Open celeste init file."
  (interactive)
  (find-file celeste-init-file))

(defun celeste/reload-init-file ()
  "Reload celeste init file."
  (interactive)
  (load-file celeste-init-file))

(defun +byte-compile-current-file ()
  "Byte compile current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file (s-suffix? "el" file))
        (byte-compile-file file)
      (message "Cannot compile current \"file\"!"))))

(defun +reload-file ()
  "Reload current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (progn (when (y-or-n-p (format "Reload %s?" file))
                 (kill-current-buffer)
                 (find-file file)))
      (message "The current buffer has no corresponding file!"))))

(keymap-set global-map "s-r" #'+reload-file)

;; Show init time after configurations are fully loaded.
(add-hook 'emacs-startup-hook
          (lambda () (message (format "Init time: %s." (emacs-init-time)))))

(defun +pure-save-buffer ()
  "Save current file w/o running `before-save-hook'."
  (interactive)
  (let (before-save-hook)
    (save-buffer)))

(defun +add-no-byte-compile-file-local-variable ()
  "Add file-local variable: `no-byte-compile': t."
  (interactive)
  (delete-file-local-variable-prop-line 'no-byte-compile)
  (add-file-local-variable 'no-byte-compile t))

(provide 'init-func)
;;; init-func.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
