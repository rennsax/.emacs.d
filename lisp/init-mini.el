;;; init-mini.el -- Minimal configurations. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Rennsax

;; Author: Rennsax <bj.ren.coding@outlook.com>
;; Maintainer: Rennsax <bj.ren.coding@outlook.com>
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This file contains the minimal defaults for Celeste Emacs. Customizations
;; declared here should work well trivially, which means:

;; 1. No third-party packages are required. Builtin packages like `bind-key',
;; `use-package' can be used (and are recommended).

;; 2. Only configure the vanilla Emacs's defaults. Builtin packages can be
;; configured, and SHOULD be.

;; 3. No appearance configurations are included. The scope of appearance:
;; colors, layout, popups... Options like `split-height-threshold' is NOT
;; considered an "appearance configuration", because it just changes the
;; behavior of window split function.

;; 4. This file should be *as stable as possible*.

;; No matter where the file is, or which entry is in the user's `load-path', the
;; file can always be loaded directly, with `eval-buffer', `load', `require', or
;; "--eval" at the command line.

;; The purpose for this file is to kickstart Emacs. GNU Emacs, an editor of at
;; least 50 years old, is even older than most of its loyal users. Therefore,
;; its default options may be not the best choice today (such as the obsolete
;; `sentence-end-double-space'). That's why we need a simple set of better
;; defaults. You can think this file as a neutral seasoning for Emacs. It aims
;; to make your Emacs more user-friendly, and should be silent and low-profile,
;; so the users even won't be aware of it.

;;; Code:

(when (version< emacs-version "29.1")
  (error "Please upgrade Emacs to at least 29.1!"))


;;; Better default options.

;; Typed text replaces the selection. Does not mean too much in `evil-mode'.
(delete-selection-mode +1)

;; If `truncate-partial-width-windows' is nil and `truncate-lines' is t, always
;; performs line truncation. By default, line truncation is enabled when the
;; window width is narrower than its number.
(setq-default truncate-partial-width-windows nil
              truncate-lines t)

;; `visual-line-mode' for soft line-wrapping. `auto-fill-mode' for hard one.
(setq-default fill-column 80 ; "fill" line when exceed 80 chars
              word-wrap t) ; wrap on whitespaces

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;; If set to t, open the *scratch* buffer.
(setq initial-buffer-choice t)

;; Do not add a string to kill-ring if it duplicates the last one, which make
;; the kill-ring easier to peruse (e.g. `consult-yank-pop').
(setq kill-do-not-save-duplicates t)

;; Emacs can create file locks to protect the file from being edited by multiple
;; users. This feature worthes little for personal computer.
(setq create-lockfiles nil)

;; TAB (`indent-for-tab-command') behavior. TAB can a) indent current line if
;; the cursor is within the beginning indentation, or b) insert literal
;; whitespaces (tab or/and space). In most major modes, TAB can insert spaces
;; and also use spaces to indent lines. In some major modes, such as
;; `makefile-mode', it's important to insert literal tab character, so these
;; modes usually set `indent-tabs-mode' to t.
(setq-default tab-always-indent nil
              indent-tabs-mode nil
              ;; Distance between each tab stop. This option is useful when
              ;; `tab-stop-list' is set to nil (the default value).
              tab-width 4)

;; Show the non-prettified version of a symbol when point is on it.
(setq prettify-symbols-unprettify-at-point t)

;; Though recommended by Emacs, it's more common to use one space after a
;; period. Ending a sentence with two spaces is considered to be a historical
;; convention.
(setq sentence-end-double-space nil)

;; Never feel getting lost where the line is when you enter CTRL-V!
(setq scroll-preserve-screen-position t)

;; Prefer simpler "y" or "n" over "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)


;;; IMPORTANT: Backup and auto-save behavior.

;; I personally enable backups, as it has little costs but can be rather
;; essential when my files are lost. Though today, most people prefer some VC
;; systems to find manage their files of different versions.
(setq make-backup-files t
      version-control t ; Use of version numbers for backup files.
      backup-by-copying t ; the default way is via mv, which may crash the FS.
      delete-old-versions t ; silently delete obsolete backups.
      ;; Recent backups.
      kept-new-versions 5
      ;; Emacs also keeps the oldest versions (w/ smallest version number), but
      ;; we need to manually remove such Paleolithic backups. They are useless
      ;; most of the time, so I'd like to make the number small.
      kept-old-versions 2)

;; Auto-save is important, especially when you accidentally kill some buffers
;; without persist your changes. Use `recover-session' and `recover-this-file'.
(setq auto-save-default t
      ;; auto-save even if a large part of the text is deleted.
      auto-save-include-big-deletions t
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))


;;; Advice, hooks.

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

;; After the file is save, guess its major mode. It can be useful when you
;; create an empty file with `find-file' (initially in `fundamental-mode') and
;; save it latter.
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

;; From minad/vertico.
;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(advice-add #'completing-read-multiple :filter-args
            (defun crm-indicator-a (args)
              "Filter the first argument of `completing-read-multiple'."
              (cons (format "[CRM%s] %s"
                            (replace-regexp-in-string
                             "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                             crm-separator)
                            (car args))
                    (cdr args))))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
;; mode.  Vertico commands are hidden in normal buffers. This setting is
;; useful beyond Vertico.
(setq read-extended-command-predicate #'command-completion-default-include-p)


;;; Builtin packages.

;; `recentf-mode': keep track of your recently-visited files.
(use-package recentf
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 240) ; just because I like this number
  (setq recentf-exclude '("\\.?cache" ".cask" "COMMIT_EDITMSG\\'"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|PNG\\|jpe?g\\|JPE?G\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "/persp-confs/" "HELLO"))
  :config
  ;; Auto-cleanup recentf after shutdown.
  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup)

  (add-to-list 'recentf-exclude
	       (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
					     "/run"))))
  ;; Filenames are shortened.
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;; `savehist-mode': persist minibuffer history to an external file.
(add-hook 'after-init-hook #'savehist-mode)
(setq history-length 1000
      ;; Additional variables to save.
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      ;; Time-based autosave of minibuffer history.
      savehist-autosave-interval 300)

;; `save-place-mode': remember where I was when I last visit this file!
(add-hook 'after-init-hook #'save-place-mode)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")


(provide 'init-mini)
;;; init-mini.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
