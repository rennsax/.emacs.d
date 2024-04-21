;;; init-cjk.el -- Better CJK supports. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const))

;; Smart input source.
(celeste/use-package sis
  :preface
  ;; `sis-context-mode' and `sis-global-respect-mode' are recommended.
  (defun +toggle-sis-mode ()
    (interactive)
    ;; Auto-switch IME according to the characters in the context.
    (sis-context-mode 'toggle)
    ;; Respect C-x, C-h, C-c, and so on.
    (sis-global-respect-mode 'toggle)
    ;; Use different cursor colors for different IME.
    (sis-global-cursor-color-mode 'toggle))
  :commands (sis-context-mode sis-global-respect-mode sis-global-cursor-color-mode)
  :config
  (setq sis-english-source "com.apple.keylayout.US"
        sis-other-source "com.sogou.inputmethod.sogou.pinyin")
  (delete "C-h" sis-prefix-override-keys)
  )

(use-package ox-latex
  :after org
  :config
  ;; Include xeCJK package so Chinese can be correctly exported.
  (add-to-list 'org-latex-packages-alist '("" "xeCJK")))


;;; `org-mode' emphasization with Chinese.
(use-package org
  :hook (org-mode . +prettify-zwsp)
  :config
  (defun +prettify-zwsp ()
    "Set prettify symbol for zero-width space."
    (setq-local prettify-symbols-alist
                '(("\u200b" . "\u02d4")))
    (prettify-symbols-mode))
  ;; Insert ZWSP easily with M-SPC.
  ;; What's M-SPC orginally? See (info "deletion").
  (keymap-set org-mode-map "M-SPC"
              #'(lambda () (interactive) (insert "\u200b")))
  )
;; When exported, remove zero-width spaces.
(use-package ox
  :preface
  :config
  (defun +org-export-remove-zwsp (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string "\u200b" "" text)))
  ;; BUG: if really removed, the text effects disappear.
  ;; (add-to-list 'org-export-filter-final-output-functions
  ;;              #'+org-export-remove-zwsp)
  )

;; TODO mixed-pitch
;; (celeste/use-package mixed-pitch)


(provide 'init-cjk)
;;; init-cjk.el ends here
