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

(when (or (executable-find "darwin-rebuild")
          (executable-find "nixos-rebuild")
          (executable-find "home-manager"))
  (let ((module-path (expand-file-name "~/.config/emacs-modules/lib")))
    (if (file-directory-p module-path)
        (add-to-list 'load-path module-path)
      (warn "You should build emacs-modules!"))))

(provide 'init-nix)
;;; init-nix.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
