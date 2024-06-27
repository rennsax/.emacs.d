;;; init-corfu.el -- Corfu completion frontend. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Lightweight completion engine, powered by capf.
;; Now only used by eshell and emacs-lisp-mode.



;;; Corfu

(celeste/prepare-package corfu "" "extensions")

(use-package corfu
  :hook (after-init . global-corfu-mode)

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
        corfu-on-exact-match nil)

  ;; No preview, i.e., do not automatically insert candidates when selected.
  (setq corfu-preview-current nil)

  ;; Disable RET -> `corfu-insert'. I prefer primitive RET - just `newline'.
  (mapc (lambda (key) (keymap-unset corfu-map key t))
        '("RET" "<remap> <next-line>" "<remap> <previous-line>"
          "<remap> <beginning-of-buffer>" "C-a"))

  ;; Enable corfu in minibuffer with `completion-at-point'.
  (progn
    (defun corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer."
      (when (local-variable-p 'completion-at-point-functions)
        ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
        (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                    corfu-popupinfo-delay nil)
        (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))
  )

(use-package corfu-history
  :hook (corfu-mode . corfu-history-mode)
  :config
  ;; Persist corfu history, utilizing the builtin `savehist'.
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Popup information about candidates.
(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(1.0 . 0.0)))

;; Quickly select candidates with prefixed chars.
(use-package corfu-quick
  :bind (:map corfu-map
              ("M-q" . corfu-quick-complete)
              ("C-q" . corfu-quick-insert)))



;;; cape: Completion At Point Extension

;; cape is another brilliant package developed by Daniel Mendler, which makes
;; capf more powerful and extensive

(celeste/prepare-package cape)

(use-package cape
  :init
  ;; NOTE: `add-hook' adds the capf to the global default value.
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)

  (with-eval-after-load 'eglot
    ;; This may hurt performance. LSP servers usually limit the completion
    ;; candidates to a small number, to avoid sending large amounts of data over
    ;; the JSONRPC protocol. If the server does not guarantee returning all
    ;; valid completions matching the prefix, you will miss potentially valid
    ;; completions. In case of doubt, use `cape-wrap-buster'.
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)

    ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
    (defun +cape-set-eglot-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         #'eglot-completion-at-point
                         #'tempel-complete))))

    ;; REVIEW: 2024-05-29 this is buggy, at least with gopls.
    ;; (add-hook 'eglot-managed-mode-hook #'+cape-set-eglot-capf)
    )

  :commands (cape-wrap-buster
             cape-capf-super
             cape-interactive)

  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  )

(use-package cape-char
  :bind (("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345)))



(provide 'init-corfu)
;;; init-corfu.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
