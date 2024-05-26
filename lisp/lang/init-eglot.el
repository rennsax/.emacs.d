;;; init-eglot.el -- Elgot setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Eglot is the out-of-box LSP client lives in Emacs itself.

(use-package eglot
  :bind (:map eglot-mode-map
              ("M-F" . eglot-format-buffer))
  :config
  (setq eglot-autoshutdown t)

  (setq eglot-stay-out-of '(flymake))

  (setq eglot-ignored-server-capabilities
        '(:colorProvider
          :foldingRangeProvider))

  ;; Enable `corfu-mode', since `eglot' kindly extends `completion-at-point'.
  (with-eval-after-load 'corfu
    (add-to-list 'eglot-managed-mode-hook #'corfu-mode))

  )

(use-package flycheck-eglot
  :after eglot
  :init
  (celeste/prepare-package flycheck)
  (celeste/prepare-package flycheck-eglot)

  :commands flycheck-eglot-mode
  :config
  ;; If `flycheck-eglot-mode' is enabled, then other checkers will be disabled.
  ;; `flycheck-eglot' implements this by set buffer-local `flycheck-checker' to
  ;; `elgot-check', so flycheck won't automatically select checkers in
  ;; `flycheck-checkers'.
  (setq-default flycheck-eglot-exclusive t))

(use-package eglot-tempel
  :after eglot
  :demand t
  :init (celeste/prepare-package eglot-tempel)
  :config
  (eglot-tempel-mode +1))


;; TODO: elgot-x?



(provide 'init-eglot)
;;; init-eglot.el ends here
