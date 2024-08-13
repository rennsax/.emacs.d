;;; init-elisp.el -- Emacs Lisp support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package elisp-mode
  :init
  (ide-zero-define elisp
      :linter default
      :mode emacs-lisp-mode)
  (add-hook 'after-init-hook #'elisp-ide-zero-mode)

  :config
  ;; use the same value as `fill-column'
  (setq emacs-lisp-docstring-fill-column nil)

  (defun advice-unadvice (sym)
    "Remove all advices from symbol SYM."
    (interactive "aFunction symbol: ")
    (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))
  )



(provide 'init-elisp)
;;; init-elisp.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
