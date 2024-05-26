;;; init-cc.el -- C/C++ support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package c-mode
  :init
  (ide-zero-define c
                   :mode (c-mode c-ts-mode)
                   :lsp ("ccls")
                   :linter lsp)
  )

(provide 'init-cc)
;;; init-cc.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
