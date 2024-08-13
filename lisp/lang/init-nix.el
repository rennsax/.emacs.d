;;; init-nix.el -- Editing Nix expressions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :init
  (celeste/package-autoload 'nix-mode)
  (ide-zero-define nix
      :mode nix-mode
      :lsp ("nil"))
  )

(provide 'init-nix)
;;; init-nix.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
