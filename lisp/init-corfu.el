;;; init-corfu.el -- Corfu completion frontend. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Lightweight completion engine, powered by capf.
;; Now only used by eshell and emacs-lisp-mode.
(celeste/use-package corfu
  :commands corfu-mode

  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  ;; Toggle corfu in all buffers

  :preface
  (defconst corfu-enable-mode-list
    '(eshell-mode emacs-lisp-mode)
    "A list of modes that `corfu-mode' should be enabled.")

  (defun +corfu-enable-corfu-mode (&optional no-auto)
    "Enable `corfu-mode' in current buffer.

If optional NO-AUTO is non-nil, turn off `corfu-auto'."
    ;; Must before `corfu-mode' is turned on.
    ;; In `eshell-mode', do not automatically toggle corfu prompt.
    (when no-auto (setq-local corfu-auto nil))
    (corfu-mode +1))

  ;; Hooks have depth -10, which means, without exceptions, they are run before
  ;; `lsp-bridge-mode' is enabled, so that `lsp-bridge-mode-hook' can detect the
  ;; conflicts and then turn `corfu-mode' off.
  (add-hook 'eshell-mode-hook (lambda () (+corfu-enable-corfu-mode t)) -10)
  (add-hook 'emacs-lisp-mode-hook #'+corfu-enable-corfu-mode -10)

  :config
  (setq corfu-auto t
        corfu-auto-prefix 3
        ;; Do not do anything (such as the default "insert") on exact match. So
        ;; a manual TAB is always needed for completion.
        corfu-on-exact-match nil
        )
  ;; Disable RET -> `corfu-insert'. I prefer primitive RET - just `newline'.
  (mapc (lambda (key) (keymap-unset corfu-map key))
        '("RET" "<remap> <next-line>" "<remap> <previous-line>"
          "<remap> <beginning-of-buffer>" "C-a"))

  )


;; Lightweight templates.
(celeste/use-package tempel
  :preface
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))
    (tempel-abbrev-mode))
  :bind (("M-*" . tempel-insert)    ; useful when no completion frontend is enabled
         :map tempel-map
         ("RET" . tempel-next)
         ("M-RET" . tempel-previous))
  :hook ((conf-mode prog-mode text-mode) . tempel-setup-capf)
  :commands (tempel-expand tempel-abbrev-mode tempel-complete)
  :config
  (setq tempel-trigger-prefix "t|")
  )

(provide 'init-corfu)
;;; init-corfu.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
