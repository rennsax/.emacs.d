;;; init-osx.el -- OS X defaults. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; NOTE: on macOS, the menu bar must be disabled after the frame is created,
;; namely, after early-init.el.
(menu-bar-mode -1)

;; Use spotlight search backend as a default for M-x locate (and helm/ivy
;; variants thereof), since it requires no additional setup.
(use-package locate
  :config
  (setq locate-command "mdfind"))

;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(and (or (daemonp)
         (display-graphic-p))
     (celeste/use-package ns-auto-titlebar
       :hook (after-init . ns-auto-titlebar-mode)))

;; Interactive calles to `delete-file' and `delete-directory' and the Dired
;; deletion commands use `move-file-to-trash'.
(setq delete-by-moving-to-trash t)

;; C-z is mapped to `suspend-frame'. In macOS, it hides the frame (with the
;; native Cocoa API) instead, which is useless.
(keymap-global-unset "C-z")

(keymap-global-set "s-Z" #'undo-redo)

(provide 'init-osx)
;;; init-osx.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
