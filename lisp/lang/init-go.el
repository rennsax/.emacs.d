;;; init-go.el -- GoLang support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package go-mode
  :init
  (celeste/prepare-package go-mode)
  :mode (("\\.go\\'" . go-mode)
         ("go\\.mod$" . go-dot-mod-mode))
  :config

  (setenv "GO111MODULE" "on")
  (setq go-mode-go-path (getenv "GOPATH"))
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
(use-package flycheck-golangci-lint
  :commands flycheck-golangci-lint-setup
  :init
  (celeste/prepare-package flycheck-golangci-lint)
  ;; Add `golangci-lint' to `flycheck-checkers'.
  (with-eval-after-load 'go-mode
    (flycheck-golangci-lint-setup)))


(provide 'init-go)
;;; init-go.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
