;;; init-window.el -- Window configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile (require 'rx))


;;; Packages.

;; `tab-bar-mode' in Emacs is much more like a window configuration stack.
(use-package tab-bar
  :custom (tab-bar-select-tab-modifiers '(super)) ; use "s-[0-9]" to switch tabs
  :config
  (setq tab-bar-show 1                 ; hide tab bar when there is only one tab
        tab-bar-close-button-show nil  ; hide the ugly close button
        tab-bar-tab-hints t            ; show number
        tab-bar-new-tab-choice #'get-scratch-buffer-create)

  (delq 'tab-bar-format-add-tab tab-bar-format))

;; Restore old window configurations.
(use-package winner
  :hook (after-init . winner-mode)
  :bind (:map window-prefix-map
              ("C-/" . winner-undo)
              ("C-?" . winner-redo)))


;; Window management routines from abo-abo.
(use-package ace-window
  :init
  (celeste/prepare-package ace-window)
  :commands (ace-window
             ace-delete-window
             ace-swap-window)
  :config
  ;; Always show dispatcher even there are only two windows.
  (setq aw-dispatch-always t)
  ;; Do not ignore any buffer.
  (setq aw-ignore-on nil)
  (setq aw-keys '(?h ?j ?k ?l ?u ?i ?o))
  (with-eval-after-load 'pulsar
    (add-to-list 'pulsar-pulse-functions #'ace-window))
  )




;; NOTE: man does not obey this `display-buffer-alist'.
(with-eval-after-load 'man
  (setq Man-notify-method 'pushy))

;; By default Emacs distinguishes between automatic and manual window switching.
;; If you effect a window switch yourself with C-x b, it’s manual — and exempt
;; from any display action rules you create yourself. *You probably don’t want
;; that*. Example: `multi-vterm'.
(setq switch-to-buffer-obey-display-actions t)

(setq display-buffer-alist
      `(;;; no window

        (,(rx bos "*Async Shell Command*" eos)
         (display-buffer-no-window))

        ;; The prompt buffer for `org-insert-link' is useless.
        (,(rx bos ?* (| "Org Links" "Compile-Log") ?* eos)
         (display-buffer-no-window)
         ;; So `display-buffer-no-window' returns non-nil ("fail").
         (allow-no-window . t))

        ;;; override the selected window
        ((or . ((derived-mode . Custom-mode)))
         (display-buffer-same-window))

        ;;; side-window

        ((or . (,(rx bos "*lsp-bridge-doc*" eos)
                ,(rx "Output*" eos)))            ; "*Pp Eval Output*, for example"
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom))

        ;; `org-capture'
        (,(rx bos "*Org Select*" eos)
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . right)
         (window-width . 0.2)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))

        ;; help/helpful
        ((or . ((derived-mode . help-mode)
                (derived-mode . helpful-mode)))
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (window-height . 0.35))

        ((or . ((derived-mode . osx-dictionary-mode)
                ,(rx bos "*devdocs*" eos)))
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (window-height . 0.3)
         (window-parameters . ((mode-line-format . none))))

        ((or . ("\\`\\*xref\\*\\'"
                "\\`\\*Abbrevs\\*\\'"))
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . 0.3)
         (dedicated . t))

        (,(rx bos "*edit-indirect " (* nonl) ?* eos)
         (display-buffer-reuse-window
          display-buffer-below-selected)
         (window-height . 0.6)
         (dedicated . t))

      ))

(keymap-global-set "C-`" #'window-toggle-side-windows)

(with-eval-after-load 'org-agenda
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote org-agenda-buffer-name)
                 (display-buffer-in-tab)
                 (ignore-current-tab . t)
                 (tab-name . "🦄")
                 (dedicated . t))))

(with-eval-after-load 'org
  (setq org-src-window-setup 'plain)
  (add-to-list 'display-buffer-alist
               `((or . (,(rx bos "*Org Src " (* nonl) ?* eos)
                        ,(rx bos "*Org Note*" eos)))
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height . 0.6)
                 (dedicated . t))))


(provide 'init-window)
;;; init-window.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
