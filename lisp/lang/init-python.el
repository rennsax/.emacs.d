;;; init-python.el -- Python support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package python
  :init
  (ide-zero-define python
      :mode (python-mode python-ts-mode)
      :lsp ("basedpyright-langserver" "--stdio")
      :linter lsp)
  :config
  (setq python-indent-offset 4)
  (defun +python-set-tab-width ()
    (setq-local tab-width python-indent-offset))
  (add-hook 'python-mode-hook #'+python-set-tab-width)
  )


(provide 'init-python)
;;; init-python.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
