;;; init-mini-extra.el -- Extra configurations for mini. -*- lexical-binding: t; -*-
;;; Commentary:

;; Extra configurations that are not specified in init-mini.el.

;; Why bother use another file to refine init-mini.el rather than including
;; these things inside init-mini.el itself? That's because I want to keep
;; init-mini.el as general as possible. Customized settings to my taste should
;; be put into another file, and this file is it.

;; In init-mini-extra.el, the restriction is relaxed a lot. Simply load this
;; file may fail, because it can require external dependencies (limited to
;; init-lib.el, init-const.el and init-package.el).

;; This file still specifies the configurations for builtin packages.

;;; Code:


;;; Data and cache directories.

;; init-mini.el has configured this builtin packages.
(setq backup-directory-alist `(("." . ,(celeste/make-path "backup/" 'cache))))
(setq save-place-file (concat celeste-cache-dir "saveplace"))
(setq savehist-file (concat celeste-cache-dir "savehist"))
(setq recentf-save-file (concat celeste-cache-dir "recentf"))
(setq auto-save-list-file-prefix (concat celeste-cache-dir "autosave/"))
;; Reuse the local backup directory.
(setq tramp-backup-directory-alist backup-directory-alist
      tramp-auto-save-directory  (concat celeste-cache-dir "tramp-autosave/"))
(setq gamegrid-user-score-file-directory (concat celeste-data-dir "games"))
(setq bookmark-default-file (concat celeste-data-dir "bookmarks"))
(setq url-cookie-file (concat celeste-cache-dir "url/cookie"))


;;; Diminished packages.

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))
(with-eval-after-load 'face-remap
  (diminish 'buffer-face-mode))


;;; Bootstrap custom file.
(setq custom-file (locate-user-emacs-file "custom.el"))
;; `load-file' vs `load': the previous one just execute the Lisp code in the
;; given file. The latter one does more things: try to add suffix, search
;; `load-path', ...
(when (file-exists-p custom-file)
  (load-file custom-file))


;;; Handle whitespaces.
(celeste/add-mode-hook '(prog-mode outline-mode conf-mode)
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


(provide 'init-mini-extra)
;;; init-mini-extra.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
