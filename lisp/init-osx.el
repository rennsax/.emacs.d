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


;;; Key bindings.

;; Map the right command to control. Mac's right command key is redundant to me.
(when sys/mac
  (setq mac-right-command-modifier 'control))

;; C-z is mapped to `suspend-frame'. In macOS, it hides the frame (with the
;; native Cocoa API) instead, which is useless.
;; "s-t" - `menu-set-font'
;; "s-p" - `ns-print-buffer'
;; "s-m" - `iconify-frame', in macOS, just minimize the frame.
(dolist (key '("C-z" "s-t" "s-p" "s-m" "s-n" "s-h"))
  (keymap-global-unset key))

(keymap-global-set "C-<wheel-up>" #'ignore)
(keymap-global-set "C-<wheel-down>" #'ignore)

;; Forbid using mouse wheel to select tab bars. My magical mouse is too
;; sensitive.
(dolist (key '("<wheel-down>"
               "<wheel-up>"
               "<wheel-right>"
               "<wheel-left>"))
  ;; Map to `ignore', so the annoying beep goes away!
  (keymap-set tab-bar-map key #'ignore))

(use-package tab-bar
  :init
  (bind-keys ("s-{" . tab-bar-switch-to-prev-tab)
             ("s-}" . tab-bar-switch-to-next-tab)
             ("s-t" . tab-bar-new-tab)
             ("s-w" . +tab-bar-close-current-tab))

  ;; It's too easy to press s-w on macOS, so I write a function that will ask
  ;; for confirmation.
  (defun +tab-bar-close-current-tab (&optional no-confirm)
    (interactive)
    (when (or no-confirm
              (y-or-n-p "Close tab?"))
      (tab-bar-close-tab))))

;; Use ctrl-shift-z to redo, so intuitive.
(keymap-global-set "s-Z" #'undo-redo)

;; Strangely, OS X cannot recognize <insert>: it seems to be translated to <help>.
(keymap-global-set "<help>" #'overwrite-mode)

(provide 'init-osx)
;;; init-osx.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
