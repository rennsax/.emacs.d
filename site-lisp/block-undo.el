;;; block-undo.el --- Undo keyboard macros in a single step    -*- lexical-binding: t; -*-

;; https://github.com/oantolin/emacs-config

(defun block-undo (fn &rest args)
  "Apply FN to ARGS in such a way that it can be undone in a single step."
  (let ((marker (prepare-change-group)))
    (unwind-protect (apply fn args)
      (undo-amalgamate-change-group marker))))

(dolist (fn '(kmacro-call-macro
              kmacro-exec-ring-item
              apply-macro-to-region-lines))
  (advice-add fn :around #'block-undo))

(provide 'block-undo)
