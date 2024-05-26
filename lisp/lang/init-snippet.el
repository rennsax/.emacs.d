;;; init-snippet.el -- Snippet engine -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Yasnippet

;; Snippet engine, integrated with Elgot, lsp-mode and lsp-bridge nicely.

(use-package yasnippet
  :init
  (celeste/prepare-package yasnippet)
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1)
  (bind-keys :map yas-keymap
             ("RET" . yas-next-field)
             ("M-RET" . yas-prev-field))
  (unbind-key "TAB" yas-keymap))



;;; Tempel

;; Lightweight templates.
(use-package tempel
  :init
  (celeste/prepare-package compat)
  (celeste/prepare-package tempel)
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



(provide 'init-snippet)
;;; init-snippet.el ends here
