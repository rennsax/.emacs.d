;;; init-editor.el -- Editor basis. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

;; Typed text replaces the selection. Does not mean too much in `evil-mode'.
(delete-selection-mode)

;; Emacs can create file locks to protect the file from being edited by multiple
;; users. This feature worthes little for personal computer.
(setq create-lockfiles nil)

;; Control the backup behavior. I personally enable backups, as it has little
;; costs but can be rather essential when my files are lost.
(setq make-backup-files t
      version-control t ; Use of version numbers for backup files.
      backup-by-copying t ; the default way is via mv, which may crash the FS.
      delete-old-versions t ; silently delete obsolete backups.
      ;; Recent backups.
      kept-new-versions 5
      ;; Emacs also keeps the oldest versions (w/ smallest version number), but
      ;; we need to manually remove such Paleolithic backups. They are useless
      ;; most of the time, so I'd like to make the number small.
      kept-old-versions 2
      backup-directory-alist `(("." . ,(expand-file-name "backup" celeste-cache-dir)))
      tramp-backup-directory-alist backup-directory-alist ; Also backup remote file locally
      )

;; TODO: `text-scale-mode'
;; (setq-default text-scale-mode-step -1)

(defun +reload-file ()
  "Reload current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (progn (kill-current-buffer)
               (find-file file))
      (message "The current buffer has no corresponding file!"))))

(keymap-set global-map "s-r" #'+reload-file)

;; TODO: auto revert
(use-package recentf
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 240) ; just because I like this number
  (setq recentf-exclude '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "/persp-confs/"))
  :custom
  (recentf-save-file (concat celeste-cache-dir "recentf"))
  :config
  (add-to-list 'recentf-exclude
	       (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
					     "/run"))))
  (add-to-list 'recentf-exclude
	       (expand-file-name recentf-save-file))
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package savehist
  :custom (savehist-file (concat celeste-cache-dir "savehist"))
  :init (setq enable-recursive-minibuffers t ; Allow commands inminibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package saveplace
  :custom (save-place-file (concat celeste-cache-dir "saveplace"))
  :hook (after-init . save-place-mode))

;; `simple' declares `size-indication-mode', `visual-line-mode',
;; `auto-fill-mode'
(use-package simple
  :hook ((after-init . size-indication-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (defvar celeste-visual-line-mode-list
    '(message-mode
      text-mode
      debugger-mode
      magit-process-mode)
    "List of modes when `visual-line-mode' should be enabled.")
  (defvar celeste-auto-fill-mode-list
    '(org-mode)
    "List of modes when `auto-fill-mode' should be enabled.")

  ;; Show column number at the modeline.
  (setq column-number-mode t
        line-number-mode nil)
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  :config
  (dolist (mode celeste-visual-line-mode-list)
    (add-hook
     (intern (concat (symbol-name mode) "-hook")) #'visual-line-mode))
  (dolist (mode celeste-auto-fill-mode-list)
    (add-hook
     (intern (concat (symbol-name mode) "-hook")) #'auto-fill-mode))
  )

;;; Format 💅🏻

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(use-package tabify
  :config
  (setq tabify-regexp "^\t* [ \t]+"))

(setq-default tab-always-indent nil ; tab can indent or insert literal indentation
              indent-tabs-mode nil ; don't insert tabs when indentation
              tab-width 4
              )

;; Show the non-prettified version of a symbol when point is on it.
(setq prettify-symbols-unprettify-at-point t)

;; `visual-line-mode' for soft line-wrapping. `auto-fill-mode' for hard one.
(setq-default fill-column 80 ; "fill" line when exceed 80 chars
              word-wrap t ; wrap on whitespaces
              truncate-lines t)

(setq truncate-partial-width-windows nil
      sentence-end-double-space nil ; an obsolete option
      )

;;; BEGIN shamelessly copied from `doom-editor.el' 8<

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `doom-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat celeste-cache-dir "autosave/")
      tramp-auto-save-directory  (concat celeste-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(add-hook 'after-save-hook
  (defun doom-guess-mode-h ()
    "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window))) ; only visible buffers
             (set-auto-mode)
             (not (eq major-mode 'fundamental-mode)))))))

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;;; >8 END shamelessly copied from doom


(provide 'init-editor)
;;; init-editor.el ends here
