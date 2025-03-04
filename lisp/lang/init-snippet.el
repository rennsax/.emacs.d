;;; init-snippet.el -- Snippet engine -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Yasnippet

;; Snippet engine, integrated with Elgot, lsp-mode and lsp-bridge nicely.

(use-package yasnippet
  :init
  (celeste/prepare-package yasnippet)
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config
  (bind-keys :map yas-keymap
             ("RET" . yas-next-field)
             ("M-RET" . yas-prev-field))
  (unbind-key "TAB" yas-keymap))



;;; Tempel

;; Lightweight templates.
;; In fact, tempel does not conflict with `yas-minor-mode'.
(use-package tempel
  :init
  (celeste/prepare-package tempel)

  (add-hook 'completion-at-point-functions #'tempel-complete)

  :bind (("M-*" . tempel-insert)    ; useful when no completion frontend is enabled
         :map tempel-map
         ("RET" . tempel-next)
         ("M-RET" . tempel-previous))
  :commands (tempel-expand tempel-abbrev-mode tempel-complete)
  :config
  (setq tempel-trigger-prefix "t|")
  )



(provide 'init-snippet)
;;; init-snippet.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
