;;; init-go.el -- GoLang support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package go-mode
  :init
  (celeste/package-autoload 'go-mode)
  (ide-zero-define go
      :mode (go-mode go-ts-mode)
      :lsp ("gopls")
      :linter lsp)

  :config
  (setenv "GO111MODULE" "on")
  (setq go-mode-go-path (getenv "GOPATH"))
  (unless go-mode-go-path
    (user-error "Cannot configure GOPATH correctly. Make sure it's set in your shell!"))

  ;; golangci-lint: Fast linters Runner for Go. It's an integrated linter suite
  ;; for GoLang. It's recommended to install it as a single binary (not by go
  ;; install).
  (celeste/require 'flycheck-golangci-lint)
  (flycheck-golangci-lint-setup)        ; Add `golangci-lint' to `flycheck-checkers'.

  )

(provide 'init-go)
;;; init-go.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
