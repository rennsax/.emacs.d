;;; init-window.el -- Window configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Initialization, global settings.

(setq display-buffer-base-action '(nil))

;; By default Emacs distinguishes between automatic and manual window switching.
;; If you effect a window switch yourself with C-x b, it’s manual — and exempt
;; from any display action rules you create yourself. *You probably don’t want
;; that*. Example: `multi-vterm'.
(setq switch-to-buffer-obey-display-actions t)

(setq switch-to-buffer-in-dedicated-window nil)

;; Cursor type in non-selected window
(setq-default cursor-in-non-selected-windows 'hollow)

(define-advice window-toggle-side-windows (:before (&optional frame) select-mru)
  "Move to the MRU window if the current window is the side window.

This avoid the cursor from moving to a random buffer when the side window is
toggled down."
  (let ((frame (window-normalize-frame frame)))
    (when (window-parameter (get-buffer-window) 'window-side)
      (select-window (get-mru-window nil nil t)))))


;;; Packages.

;; `tab-bar-mode' in Emacs is much more like a window configuration stack.
(use-package tab-bar
  :custom (tab-bar-select-tab-modifiers '(super)) ; use "s-[0-9]" to switch tabs
  :config
  (setq tab-bar-show 1                 ; hide tab bar when there is only one tab
        tab-bar-close-button-show nil  ; hide the ugly close button
        tab-bar-tab-hints t            ; show number
        tab-bar-new-tab-choice #'get-scratch-buffer-create)

  ;; It's inappropriate to use the side effect of `delq'. Better choice: use
  ;; `remq', which does not modify the list.
  (setq tab-bar-format
        (delq 'tab-bar-format-add-tab tab-bar-format))
  )

;; Restore old window configurations.
(use-package winner
  :hook (after-init . winner-mode)
  :bind (:map window-prefix-map
              ("C-/" . winner-undo)
              ("C-?" . winner-redo)))


;; Window management routines from abo-abo.
(use-package ace-window
  :commands (ace-window
             ace-delete-window
             ace-swap-window
             ace-display-buffer
             aw-select)

  :init
  (celeste/prepare-package ace-window)
  ;; Switch to the window that already displays the buffer, otherwise use
  ;; `aw-select' to choose a window interactively.
  (setq display-buffer-base-action '((display-buffer-reuse-window
                                      ace-display-buffer)))

  (defun ace-copy-window ()
    "Ace copy window."
    (interactive)
    (aw-select " Ace - Copy Window" #'aw-copy-window))

  :hook (after-init . ace-window-display-mode)
  :config
  ;; Always show dispatcher even there are only two windows.
  (setq aw-dispatch-always t)
  ;; Do not ignore any buffer.
  (setq aw-ignore-on nil)
  (setq aw-keys '(?h ?j ?k ?l ?u ?i ?o))

  (with-eval-after-load 'pulsar
    (setq pulsar-pulse-functions
          (append pulsar-pulse-functions
                  '(ace-window ace-display-buffer
                               ace-copy-window ace-swap-window
                               ace-delete-window ace-delete-other-windows))))
  ;; (delete-dups pulsar-pulse-functions)

  ;; When `aw-select', hide cursors in all windows.
  (define-advice aw-select (:around (fun &rest r) hide-cursor)
    (let ((cursor-in-non-selected-windows nil))
      (apply fun r)))

  )

;; Add Embark integration powered by `ace-window'.
(defmacro embark--ace-window-call (fun)
  "Select a window with `aw-select', then call FUN."
  `(lambda (arg) (interactive "P")
     (with-demoted-errors "%S"
       (let ((aw-dispatch-always t)
             (win-list (aw-window-list)))
         ;; If there is less than 2 windows, split a window first.
         (when (length< win-list 2)
           (if arg (split-window-below) (split-window-right)))
         (aw-select " Ace - Embark Select" #'aw-switch-to-window)
         (call-interactively #',fun)))))

(with-eval-after-load 'embark
  ;; Alternatives for `*-other-window'.
  (keymap-set embark-file-map "o" (embark--ace-window-call find-file))
  (keymap-set embark-buffer-map "o" (embark--ace-window-call switch-to-buffer))
  (keymap-set embark-bookmark-map "o" (embark--ace-window-call bookmark-jump))
  (keymap-set embark-symbol-map "o" (embark--ace-window-call embark-find-definition))
  )


(defun display-buffer-base-action--empty-wrapper-a (fun &rest args)
  "Set `display-buffer-base-action' to (nil) before calling FUN."
  (let ((display-buffer-base-action (list nil)))
    (apply fun args)))

(defmacro display-buffer-base-empty-wrap (feature fun)
  "After loading FEATURE, protect FUN with plain `display-buffer-base-action'.

FUN is the symbol of the function declared by FEATURE, or a lambda expression
that returns the symbol."
  `(with-eval-after-load ',feature
     (advice-add ,(cond ((and (symbolp fun)
                              (fboundp fun))
                         `(quote ,fun))
                          ((and (listp fun)
                                (eq (car fun) 'lambda))
                           `(funcall ,fun))
                          (t (user-error "Unable to handle %s" fun)))
                 :around #'display-buffer-base-action--empty-wrapper-a)))

(display-buffer-base-empty-wrap consult consult-buffer-other-window)
(display-buffer-base-empty-wrap magit (lambda () magit-display-buffer-function))
(display-buffer-base-empty-wrap org-capture org-capture)
(display-buffer-base-empty-wrap embark embark-act)



;; NOTE: man does not obey this `display-buffer-alist'.
(with-eval-after-load 'man
  (setq Man-notify-method 'pushy))

;; Tips:

;; 1. Use `(body-function . select-window)' for transient (not transient.el)
;; windows. Typically I want to press "q" immediately to get rid of them.

;; 2. Set `window-min-height'. Some buffers provide extra information for the
;; current buffer, so I just want to display that below, always! Setting
;; `window-min-height' to a small number (e.g. 2) guarantees a window is created
;; below at most of the situations.

(setq display-buffer-alist
      `(

;;; no window

        (,(rx bos "*Async Shell Command*" eos)
         (display-buffer-no-window))

        ;; The prompt buffer for `org-insert-link' is useless.
        (,(rx bos ?* (| "Org Links" "Compile-Log") ?* eos)
         (display-buffer-no-window)
         ;; So `display-buffer-no-window' returns non-nil ("fail").
         (allow-no-window . t))

;;; override the selected window (same window)

        ((or . ((derived-mode . Custom-mode)
                (derived-mode . Buffer-menu-mode)
                (derived-mode . deadgrep-mode)
                (derived-mode . epa-key-mode)
                (derived-mode . epa-key-list-mode)
                (derived-mode . forge-post-mode)))
         (display-buffer-reuse-mode-window
          display-buffer-same-window))

        ;; REVIEW: `info-lookup' is weird. To pop to a window for "*info*"
        ;;   buffer, it:
        ;;   1. Call `info' once with `save-window-excursion'.
        ;;   2. Pop/switch to the window/buffer according to the result.
        ;;   I do not quite understand this routine now. As a result, it will
        ;;   call `display-buffer' twice, and if the current window is a dedicated
        ;;   one, it triggers my `display-buffer-base-action' twice, which is
        ;;   as I need to `ace-select-window' twice.
        ;;
        ;;   Therefore, the current workaround is to override its
        ;;   `display-buffer' action.
        (,(rx bos "*info*" eos)
         (display-buffer-reuse-mode-window
          display-buffer-same-window
          display-buffer-use-some-window))

;;; side-window

        ((or . (,(rx bos "*lsp-bridge-doc*" eos)
                ,(rx "Output*" eos)     ; "*Pp Eval Output*, for example"
                ,(rx bos "*Messages*" eos)))
         (display-buffer-in-side-window)
         (side . bottom))

        ((or . (,(rx bos "*envrc*" eos)
                ,(rx bos "*tex-shell*" eos)
                ,(rx bos "*compilation*" eos)))
         (display-buffer-in-side-window)
         (side . bottom)
         (body-function . select-window))

        ;; `org-capture'
        (,(rx bos "*Org Select*" eos)
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.2)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))

        ;; `org-agenda' dispatcher (there is a leading space!)
        (,(rx bos " *Agenda Commands*" eos)
         (display-buffer-in-side-window)
         (side . bottom)
         (window-width . 0.3)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))

        ("\\*elfeed-entry\\*"
         (display-buffer-in-tab)
         (tab-name . (lambda (buffer alist)
                       (with-current-buffer buffer
                         (concat "🚀 " (elfeed-feed-title (elfeed-entry-feed elfeed-show-entry))))))
         (tab-group . "📻 Elfeed"))

        ("\\*elfeed-search\\*"
         (display-buffer-in-tab)
         (tab-name . "📣 Entries")
         (tab-group . "📻 Elfeed"))

        ;; help/helpful
        ((or . ((derived-mode . help-mode)
                (derived-mode . helpful-mode)
                ,(rx bos "*eldoc*" eos)))
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.35)
         (body-function . select-window))

;;; Below the current window

        ((or . ("\\`\\*xref\\*\\'"
                "\\`\\*Abbrevs\\*\\'"))
         (display-buffer-reuse-mode-window
          display-buffer-below-selected
          display-buffer-same-window)
         (window-min-height . 8)
         (dedicated . t))

        ((or . (,(rx bos "*Warnings*" eos)
                (derived-mode . debugger-mode)))
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-min-height . 2)
         (window-height body-lines . 16)
         (dedicated . t)
         (body-function . select-window))

        (,(rx bos "*edit-indirect " (* nonl) ?* eos)
         (display-buffer-reuse-window
          display-buffer-below-selected)
         (window-height . 0.4)
         (dedicated . t))

        ((or . (,(rx bos "*Org Src " (* nonl) ?* eos)
                ,(rx "Org Note")
                (derived-mode . calendar-mode)))
         (display-buffer-reuse-window
          display-buffer-below-selected)
         (window-height . 0.4)
         (dedicated . t))

      ))

(keymap-global-set "C-`" #'window-toggle-side-windows)

;; Many commands of `org-mode' do not respect `display-buffer-alist'. They
;; prefer to use their own options to customize the behavior of new buffer, for
;; example, by `org-agenda-window-setup' we can configure how the new
;; `org-agenda' buffer shows up, if instead of using it we manually configure
;; `display-buffer-alist', then `org-agenda-quit' may call `delete-window',
;; which break the window configuration.
(with-eval-after-load 'org
  (setq org-src-window-setup 'plain))

;; Buffers with dedicated windows should be hidden in `consult-buffer', or
;; consulting them will break the window configuration.
(with-eval-after-load 'consult
  (setq consult-buffer-filter
        `("\\` " "\\`\\*Completions\\*\\'" "\\`\\*Flymake log\\*\\'" "\\`\\*Semantic SymRef\\*\\'" "\\`\\*tramp/.*\\*\\'" ; original
          "[Oo]utput\\*\\'"
          "\\`\\*\\(?:Async Shell Command\\|Messages\\|Warnings\\|Compile-Log\\|Compilation\\)\\*\\'"
          "\\*helpful.*\\*" "\\*Help\\*"
          "\\*lsp-bridge-doc\\*" "\\*Flycheck checkers\\*"
          "\\*osx-dictionary\\*"
          "\\`\\*Backtrace\\*\\'"
          "\\`\\*Disabled Command\\*\\'"
          "\\`\\*Flycheck checker\\*\\'"
          ,(rx bos "*Org Src" (* nonl))
          "\\`\\*Calendar\\*\\'"
          ,(rx bos "*elfeed-" (| "search" "entry") "*" eos)
          ,(rx bos "*compilation*" eos)
          ,(rx bos "*envrc*" eos)
          ,(rx bos "*tex-shell*" eos)
          ))
  )


(provide 'init-window)
;;; init-window.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
