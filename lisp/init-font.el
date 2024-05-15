;;; init-font.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Font settings.
;; Useful hooks:
;; `after-init-hook': not helpful in daemon mode.
;; `server-after-make-frame-hook': triggered when the daemon create a new frame.
;; `text-scale-mode-hook': TODO
;; `after-setting-font-hook': after the frame font is *changed*.

(create-fontset-from-fontset-spec
 (format  "-*-%s-regular-normal-normal-*-%d-*-*-*-p-0-fontset-celeste"
          celeste-default-font-name celeste-font-size))

;; This workaround is found at https://emacs-china.org/t/doom-emacs/23513
;; See the variable `char-script-table'.
(defun +fontset-setup-cjk (&optional fontset)
  "Setup special CJK fonts for FONTSET."
  (dolist (charset '(kana han cjk-misc bopomofo symbol))
    (set-fontset-font fontset charset (font-spec :family celeste-cjk-font-name))))

;; Patch the fontset after `buffer-face-mode' is enabled.
(add-hook 'celeste-buffer-face-mode-hook #'+fontset-setup-cjk)

;; `after-setting-font-hook' isn't triggered since the font of the initial frame
;; is never *changed*.
(+fontset-setup-cjk "fontset-celeste")

(push '(font . "fontset-celeste") default-frame-alist)


;;; Magical multi-font settings.

(defun celeste/buffer-set-other-font (&optional font-family no-hook)
  "Setup another font for the current buffer.

If FONT-FAMILY is non-nil, use the specified font. Otherwise,
`celeste-other-font-name' is used.

If NO-HOOK is non-nil, by passing the execution of
`celeste-buffer-face-mode-hook'."
  (interactive "MFont family: ")
  (let ((font-family (or font-family celeste-other-font-name)))
    (if (string= font-family celeste-default-font-name)
        (buffer-face-mode -1)
      (progn
        (setq-local buffer-face-mode-face `(:family ,font-family))
        (buffer-face-mode +1)
        (unless no-hook
            (run-hooks 'celeste-buffer-face-mode-hook))))))

;; Set different fonts for those special modes, so I can distinguish from
;; different contexts.
(celeste/add-mode-hook celeste-other-font-mode-list #'celeste/buffer-set-other-font)

(celeste/add-mode-hook celeste-cjk-font-mode-list
    #'(lambda () (celeste/buffer-set-other-font celeste-cjk-font-name 'no-hook)))

(provide 'init-font)
;;; init-font.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
