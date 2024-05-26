;;; init-lsp-bridge.el -- Lsp-bridge setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; LSP

(use-package lsp-bridge
  :diminish lsp-bridge-mode
  :commands lsp-bridge-mode global-lsp-bridge-mode
  :config
  ;; Enable tempel.
  (with-eval-after-load 'tempel
    (setq acm-enable-tempel t))
  ;; RET should just insert a newline, completion is done by TAB.
  (mapc (lambda (key) (keymap-unset acm-mode-map key))
        '("RET" "<remap> <next-line>" "<remap> <previous-line>"
          "<remap> <beginning-of-buffer>"))

  ;; corfu-mode is conflicted with lsp-bridge-mode.
  (with-eval-after-load 'corfu
    (add-hook 'lsp-bridge-mode-hook
              (defun +lsp-bridge-mode-repel-corfu-mode-h (&rest _)
                (if lsp-bridge-mode
                    (corfu-mode -1)
                  (when (and (boundp 'corfu-enable-mode-list)
                             (apply #'derived-mode-p corfu-enable-mode-list))
                    (corfu-mode +1))))))

  ;; VSCode-style key bindings
  (bind-keys :map lsp-bridge-mode-map
             ("<f12>" . lsp-bridge-find-def)
             ("S-<f12>" . lsp-bridge-find-def-return)
             ("<f2>" . lsp-bridge-rename)
             ("M-F" . lsp-bridge-code-format))
  ;; Conflict with my `upcase-previous-word'.
  (keymap-unset acm-mode-map "M-u" t)

  :init
  (celeste/prepare-package lsp-bridge)
  (celeste/prepare-package markdown-mode)
  (celeste/prepare-package yasnippet)

  )



(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
