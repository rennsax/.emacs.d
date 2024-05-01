;;; init-cjk.el -- Better CJK supports. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Allowing breaking after CJK characters and improves the word-wrapping for CJK
;; text mixed with Latin text.
(setq word-wrap-by-category t)

;; Smart input source.
(celeste/use-package sis
  :demand t
  :config
  ;; Make sure your input sources are these two (hint: use macism)
  (setq sis-english-source "com.apple.keylayout.US"
        sis-other-source "com.sogou.inputmethod.sogou.pinyin")
  (pcase celeste-modal-editing
    ('evil
     (with-eval-after-load 'evil
       ;; Auto-switch IME according to the characters in the context.
       (sis-global-context-mode +1)
       ;; Respect C-x, C-h, C-c, and so on.
       (sis-global-respect-mode +1)
       ;; Use different cursor colors for different IME.
       (sis-global-cursor-color-mode +1)))
    ('meow
     (with-eval-after-load 'meow
       (add-hook 'meow-insert-exit-hook #'sis-set-english)
       (setq sis-context-hooks '(meow-insert-enter-hook))
       (sis-global-context-mode +1)
       (sis-global-cursor-color-mode +1)
       (add-hook 'meow-insert-enter-hook #'(lambda () (sis-global-respect-mode +1)))
       (add-hook 'meow-insert-exit-hook #'(lambda () (sis-global-respect-mode -1)))
       ))
    (_
     (sis-global-context-mode +1)
     (sis-global-respect-mode +1)
     (sis-global-cursor-color-mode +1))))

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

;; Local Variables:
;; no-byte-compile: t
;; End:
