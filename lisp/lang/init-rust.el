;;; init-rust.el -- Rust support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :init (celeste/prepare-package rust-mode)
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (celeste/setup-lang rust
    :modes (rust-mode)
    :flycheck eglot
    :add-hook t))

(provide 'init-rust)
;;; init-rust.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
