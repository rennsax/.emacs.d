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
     (use-package ns-auto-titlebar
       :init
       (celeste/prepare-package ns-auto-titlebar)
       :hook (after-init . ns-auto-titlebar-mode)))

;; Interactive calles to `delete-file' and `delete-directory' and the Dired
;; deletion commands use `move-file-to-trash'.
(setq delete-by-moving-to-trash t)

;; C-z is mapped to `suspend-frame'. In macOS, it hides the frame (with the
;; native Cocoa API) instead, which is useless.
;; "s-t" - `menu-set-font'
;; "s-p" - `ns-print-buffer'
;; "s-m" - `iconify-frame', in macOS, just minimize the frame.
(dolist (key '("C-z" "s-t" "s-p" "s-m" "s-n"
               "C-<wheel-up>" "C-<wheel-down>"))
  (keymap-global-unset key))

(use-package tab-bar
  :init
  (bind-keys ("s-{" . tab-bar-switch-to-prev-tab)
             ("s-}" . tab-bar-switch-to-next-tab)
             ("s-t" . tab-bar-new-tab)))

;; Use ctrl-shift-z to redo, so intuitive.
(keymap-global-set "s-Z" #'undo-redo)

(provide 'init-osx)
;;; init-osx.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
