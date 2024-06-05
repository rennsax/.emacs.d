;;; init-python.el -- Python support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package python-mode
  :init
  (ide-zero-define python
                   :mode (python-mode python-ts-mode)
                   ;; pyright
                   :lsp ("pyright-langserver" "--stdio") ;; Use basedpyright?
                   :linter lsp)
  :config
  (setq python-indent-offset 4)
  )


(provide 'init-python)
;;; init-python.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
