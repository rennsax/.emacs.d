;;; init-xref.el -- Xref setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;;; Xref provides cross-referencing commands for Emacs.


(use-package xref
  :config
  (define-advice xref-goto-xref (:filter-args (&optional quit) always-quit) '(t))
  (setq xref-search-program 'ripgrep)
  )



(provide 'init-xref)
;;; init-xref.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
