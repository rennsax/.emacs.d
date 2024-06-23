;;; init-mini-extra.el -- Extra configurations for mini. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; Data and cache directories.

;; There is a package named "no-littering" which does the same stuff. But I
;; prefer to write these on my own, for less external dependencies and more
;; transparent behavior.

;; init-mini.el has configured this builtin packages.
(setq backup-directory-alist `(("." . ,(celeste/make-path "backup/" 'cache))))
(setq save-place-file (concat celeste-cache-dir "saveplace"))
(setq savehist-file (concat celeste-cache-dir "savehist"))
(setq recentf-save-file (concat celeste-cache-dir "recentf"))
(setq auto-save-list-file-prefix (celeste/make-path "autosave/" 'cache)
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))
;; Reuse the local backup directory.
(setq tramp-backup-directory-alist backup-directory-alist
      tramp-auto-save-directory  (concat celeste-cache-dir "tramp-autosave/"))
(setq gamegrid-user-score-file-directory (concat celeste-data-dir "games"))
(setq bookmark-default-file (concat celeste-data-dir "bookmarks"))
(setq url-cookie-file (concat celeste-cache-dir "url/cookie"))
(setq project-list-file (celeste/make-path "projects" 'data))


;;; Diminished packages.

(diminish 'auto-fill-function)
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))
(with-eval-after-load 'face-remap
  (diminish 'buffer-face-mode))
(with-eval-after-load 'subword
  (diminish 'subword-mode))
(with-eval-after-load 'hideshow
  (diminish 'hs-minor-mode))


(celeste/add-mode-hook '(prog-mode text-mode)
    (defun +set-show-trailing-whitespace ()
      (setq-local show-trailing-whitespace t)))


;;; Handle line wrap in different modes.

;; HACK: the "*Message*" buffer has been created before, so `add-hook' to
;; `message-mode-hook' does not help.
(with-current-buffer "*Messages*" (visual-line-mode))

(defvar celeste-visual-line-mode-list
  '(message-mode
    debugger-mode
    compilation-mode
    eshell-mode
    magit-process-mode)
  "List of modes when `visual-line-mode' should be enabled.")

(defvar celeste-auto-fill-mode-list
  '(org-mode)
  "List of modes when `auto-fill-mode' should be enabled.")

(define-minor-mode celeste/auto-fill-mode
  "Toggle `auto-fill-mode' and `display-fill-column-indicator-mode' together."
  :init-value nil
  :global nil
  (if celeste/auto-fill-mode
      (progn
        (auto-fill-mode +1) (display-fill-column-indicator-mode +1))
    (auto-fill-mode -1)
    (display-fill-column-indicator-mode -1)))

(celeste/add-mode-hook celeste-visual-line-mode-list #'visual-line-mode)
(celeste/add-mode-hook celeste-auto-fill-mode-list #'celeste/auto-fill-mode)



;;; Some files are opened to be read-only for protection.

(defcustom celeste-readonly-file-regexp
  `(,lisp-directory)
  "Regexp of file names that should be read-only when visited."
  :group 'celeste
  :type '(repeat regexp))

(add-hook 'find-file-hook
          (defun +file-set-readonly ()
            (when-let ((buf-name (buffer-file-name)))
              (when
                  (seq-some (lambda (reg) (string-match-p reg buf-name))
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
