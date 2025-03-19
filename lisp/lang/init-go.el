;;; init-go.el -- GoLang support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package go-mode
  :init
  (celeste/package-autoload 'go-mode)
  :config
  (setenv "GO111MODULE" "on")
  (setq go-mode-go-path (getenv "GOPATH"))
  (unless go-mode-go-path
    (user-error "Cannot configure GOPATH correctly. Make sure it's set in your shell!"))

  ;; golangci-lint: Fast linters Runner for Go. It's an integrated linter suite
  ;; for GoLang. It's recommended to install it as a single binary (not by go
  ;; install).
  (celeste/prepare-package flycheck-golangci-lint)
  (require 'flycheck-golangci-lint)
  (flycheck-golangci-lint-setup)        ; Add `golangci-lint' to `flycheck-checkers'.

  (celeste/setup-lang go
    :modes (go-mode go-ts-mode)
    :eglot-server ("gopls")
    :flycheck eglot
    :project-identify "go.mod")
  )

(provide 'init-go)
;;; init-go.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
