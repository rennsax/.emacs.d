;;; init-yaml.el -- YAML support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const))

(celeste/use-package yaml-mode
  ;; TODO: This causes duplicated entries in `auto-mode-alist'.
  :mode "\\.ya?ml\\'"
  :commands yaml-mode
  :config
  (setq yaml-indent-offset 2)
  ;; Adjust `tab-width' (buffer-local)
  (celeste/add-mode-hook '(yaml-mode yaml-ts-mode)
      (defun +yaml-mode-set-tab-width ()
          "Set tab width for yaml mode"
          (setq tab-width yaml-indent-offset)))
  )

(provide 'init-yaml)
;;; init-yaml.el ends here
