;;; init-yaml.el -- YAML support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :init
  (celeste/package-autoload 'yaml-mode)

  :config
  (setq yaml-indent-offset 2)
  ;; Adjust `tab-width' (buffer-local). Hooks must be added before `yaml-mode'
  ;; is loaded. Otherwise the first opened yaml file's `tab-width' is not set.
  (celeste/add-mode-hook '(yaml-mode yaml-ts-mode)
      (defun +yaml-mode-set-tab-width ()
          "Set tab width for yaml mode"
          (setq tab-width yaml-indent-offset)))

  )

(provide 'init-yaml)
;;; init-yaml.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
