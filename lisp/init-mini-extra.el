;;; init-mini-extra.el -- Extra configurations for mini. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; Data and cache directories.

;; There is a package named "no-littering" which does the same stuff. But I
;; prefer to write these on my own, for less external dependencies and more
;; transparent behavior.

;; init-mini.el has configured this builtin packages.
(setq backup-directory-alist `(("." . ,(celeste/make-path "backup/" 'cache))))
(setq save-place-file (celeste/make-path "saveplace" 'state))
(setq savehist-file (celeste/make-path "savehist" 'state))
(setq recentf-save-file (celeste/make-path "recentf" 'state))
(setq auto-save-list-file-prefix (celeste/make-path "autosave/" 'cache)
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))
;; Reuse the local backup directory. According to the implementation of
;; `tramp-handle-find-backup-file-name', if `tramp-backup-directory-alist' is
;; set then the actual backup directory will be prepended with the Tramp file
;; name prefix. If it's nil then `backup-directory-alist' is used, so the backup
;; is stored locally.
(setq tramp-backup-directory-alist nil
      tramp-auto-save-directory  (celeste/make-path "tramp-autosave/" 'cache)
      tramp-persistency-file-name (celeste/make-path "tramp/connection-history" 'state))
(setq gamegrid-user-score-file-directory (celeste/make-path "games/" 'state))
(setq bookmark-default-file (celeste/make-path "bookmarks" 'state))
(setq url-configuration-directory (celeste/make-path "url/" 'data)
      url-cache-directory (celeste/make-path "url-cache/" 'cache)
      url-cookie-file (celeste/make-path "url-cookie" 'state))
(setq eshell-history-file-name (celeste/make-path "eshell/history" 'state)
      eshell-last-dir-ring-file-name (celeste/make-path "eshell/lastdir" 'state))
(setq project-list-file (celeste/make-path "projects" 'state))
(setq type-break-file-name (celeste/make-path "type-break" 'state))


;;; Diminished packages.

(diminish 'auto-fill-function)
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))
(with-eval-after-load 'face-remap
  (diminish 'buffer-face-mode))
(with-eval-after-load 'hideshow
  (diminish 'hs-minor-mode))


(celeste/add-mode-hook '(prog-mode text-mode)
    (defun +set-show-trailing-whitespace ()
      (setq-local show-trailing-whitespace t)))


;;; Handle line wrap in different modes.

;; HACK: the "*Message*" buffer has been created before, so `add-hook' to
;; `message-mode-hook' does not help.
(with-current-buffer "*Messages*" (visual-line-mode))

(defcustom celeste-visual-line-mode-list
  '(message-mode
    debugger-mode
    compilation-mode
    eshell-mode)
  "List of modes when `visual-line-mode' should be enabled."
  :group 'celeste
  :type '(repeat symbol)
  :set (celeste--mode-list-setter visual-line-mode))

(define-minor-mode celeste/auto-fill-mode
  "Toggle `auto-fill-mode' and `display-fill-column-indicator-mode' together."
  :init-value nil
  :global nil
  (if celeste/auto-fill-mode
      (progn
        (auto-fill-mode +1) (display-fill-column-indicator-mode +1))
    (auto-fill-mode -1)
    (display-fill-column-indicator-mode -1)))

(defcustom celeste-auto-fill-mode-list
  '(org-mode)
  "List of modes when `auto-fill-mode' should be enabled."
  :group 'celeste
  :type '(repeat symbol)
  :set (celeste--mode-list-setter celeste/auto-fill-mode))


;;; Some files are opened to be read-only for protection.

(defcustom celeste-readonly-file-regexp
  `(,lisp-directory)
  "Regexps of file names that should be read-only when visited.

If the regexp is a directory name (determined by `directory-name-p'), then
`file-in-directory-p' is also used for checking."
  :group 'celeste
  :type '(repeat regexp))

(add-hook 'find-file-hook
          (defun +file-set-readonly ()
            (when-let ((file-name (buffer-file-name)))
              (when
                  (seq-some (lambda (dir-or-rep)
                              (or
                               (and (directory-name-p dir-or-rep)
                                    (file-in-directory-p file-name dir-or-rep))
                               (string-match-p dir-or-rep file-name)))
                            celeste-readonly-file-regexp)
                (read-only-mode)))))


;;; Misc.

;; Always write buffer content to the file when `save-buffer', because
;; sometimes I need to save the buffer for cleaning up whitespaces, even when
;; it has not been modified.
(defun +save-buffer-alway-a (&rest _)
  (set-buffer-modified-p t))
(define-minor-mode celeste/always-save-buffer-mode
  "Always save buffer even `buffer-modified-p' return nil."
  :global t
  :keymap nil
  (if celeste/always-save-buffer-mode
      (advice-add #'save-buffer :before #'+save-buffer-alway-a)
    (advice-remove #'save-buffer #'+save-buffer-alway-a)))


(provide 'init-mini-extra)
;;; init-mini-extra.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
