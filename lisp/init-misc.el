;;; init-misc.el -- Miscellaneous configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; Handle whitespaces.
(celeste/add-mode-hook '(prog-mode markdown-mode conf-mode)
    (defun enable-trailing-whitespace ()
      "Show trailing spaces and delete on saving."
      (setq show-trailing-whitespace t)
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local)))


;;; Handle line wrap in different modes.

;; HACK: the "*Message*" buffer has been created before, so `add-hook' to
;; `message-mode-hook' does not help.
(with-current-buffer "*Messages*" (visual-line-mode))

;; Celeste Emacs has presets for modes in which `visual-line-mode' and
;; `auto-fill-mode' should be enabled.
(celeste/add-mode-hook celeste-visual-line-mode-list #'visual-line-mode)
(celeste/add-mode-hook celeste-auto-fill-mode-list #'auto-fill-mode)

;; Toggle `display-fill-column-indicator-mode' along with `auto-fill-mode'.
;; Show vertical line at the column of `fill-column'.
;; TODO: how to tell whether `auto-fill-mode' is enabled?
(add-hook 'auto-fill-mode-hook
          #'(lambda () (display-fill-column-indicator-mode 'toggle)))


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

(provide 'init-misc)
;;; init-misc.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
