;;; init-python.el -- Python support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package python
  :config
  (setq python-indent-offset 4)
  (defun +python-set-tab-width ()
    (setq-local tab-width python-indent-offset))
  (add-hook 'python-mode-hook #'+python-set-tab-width)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (celeste/setup-lang python
    :modes (python-mode python-ts-mode)
    :eglot-server ("basedpyright-langserver" "--stdio")
    :flycheck eglot
    :project-identify "pyproject.toml"
    :add-hook t)
  )


(provide 'init-python)
;;; init-python.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
