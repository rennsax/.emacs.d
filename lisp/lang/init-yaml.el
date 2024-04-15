;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'init-const))

(celeste/use-package yaml-mode
  ;; TODO: This causes duplicated entries in `auto-mode-alist'.
  :mode "\\.ya?ml\\'"
  :commands yaml-mode
  :config
  (setq yaml-indent-offset 2)
  ;; Adjust `tab-width' (buffer-local)
  (add-hook 'yaml-mode-hook #'(lambda () (setq tab-width yaml-indent-offset))))

(provide 'init-yaml)
