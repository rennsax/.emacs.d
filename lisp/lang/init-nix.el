;;; init-nix.el -- Editing Nix expressions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :init
  (celeste/package-autoload 'nix-mode)
  (celeste/setup-lang nix
    :modes (nix-mode)
    :eglot-server ("nil")
    :project-identify "flake.nix"
    :eglot t
    :flycheck eglot
    :add-hook t)
  )

(provide 'init-nix)
;;; init-nix.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
