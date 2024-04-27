;;; init-go.el -- GoLang support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

(celeste/use-package go-mode
  :mode (("\\.go\\'" . go-mode)
         ("go\\.mod$" . go-dot-mod-mode))
  :init
  (defvar go-mode-go-path)
  :config

  (setq go-mode-go-path (+getenv-shell "GOPATH"))
  (unless go-mode-go-path
    (user-error "Cannot configure GOPATH correctly. Make sure it's set in your shell!"))

  ;; Remove default Go checkers from `flycheck-checkers'. We use `golangci-lint'
  ;; instead.
  (with-eval-after-load 'flycheck
    (setq flycheck-checkers
          (seq-remove
           (lambda (symbol) (string-match "go-.*" (symbol-name symbol)))
           flycheck-checkers)))

  ;; cmd/guru is obsoleted by gopls since March 2024. So the package `go-guru'
  ;; is deprecated.
  )

;; golangci-lint: Fast linters Runner for Go. It's an integrated linter suite
;; for GoLang. It's recommended to install it as a single binary (not by go
;; install).
(celeste/use-package flycheck-golangci-lint
  ;; Add `golangci-lint' to `flycheck-checkers'.
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setenv "GO111MODULE" "on")
  )


(provide 'init-go)
;;; init-go.el ends here
