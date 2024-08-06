;;; init-basic.el -- Minimal setup for builtins -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "29.1")
  (error "Please upgrade Emacs to at least 29.1!"))

;; Key bindings.
(use-package emacs
  :init
  ;; Key bindings for Emacs builtin features.
  ;; NOTE: Bindings for third-party packages should be put into their
  ;; corresponding sections (their `use-package' configurations, for example).
  (bind-keys ("M-J" . join-line)
             ("C-S-n" . scroll-up-line)
             ("C-S-p" . scroll-down-line)
             ("C-c b s" . scratch-buffer)
             ("s-o" . other-window)
             :map prog-mode-map
             ("M-RET" . comment-indent-new-line))

  ;; Increasing or decreasing the default font size in all GUI Emacs
  ;; frames. (new feature in Emacs 29.1)
  ;; TODO: reset with "s-0"
  (bind-keys ("s-+" . (lambda () (interactive) (global-text-scale-adjust 1)))
             ("s-_" . (lambda () (interactive) (global-text-scale-adjust -1))))
  )

;; Bootstrap custom file.
;; NOTE: Should be as early as possible
(use-package cus-edit
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  ;; `load' is more primitive than `load-file'.
  (load custom-file t t t))

;; Editor setups.
(use-package emacs
  :init
  ;; If `truncate-partial-width-windows' is nil and `truncate-lines' is t, always
  ;; performs line truncation. By default, line truncation is enabled when the
  ;; window width is narrower than its number.
  (setq-default truncate-partial-width-windows nil
                truncate-lines t)

  ;; `visual-line-mode' for soft line-wrapping. `auto-fill-mode' for hard one.
  (setq-default fill-column 80
                word-wrap t)

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

  ;; Emacs can create file locks to protect the file from being edited by multiple
  ;; users. This feature worthes little for personal computer.
  (setq create-lockfiles nil)

  ;; Never feel getting lost where the line is when you enter CTRL-V!
  (setq scroll-preserve-screen-position 1)

  ;; Prefer simpler "y" or "n" over "yes" or "no".
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Confirm before `save-buffers-kill-emacs'.
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Also apply dir-local variables to remote files.
  (setq enable-remote-dir-locals t)

  ;; https://emacs.stackexchange.com/questions/72462/failed-to-trash-remote-directories-over-tramp
  (define-advice system-move-file-to-trash (:override (filename) use-trash-cli)
    (process-file-shell-command
     (format "trash %S" (file-local-name filename))))
  )

;; Backup and auto-save.
(use-package emacs
  :init
  ;; I personally enable backups, as it has little costs but can be rather
  ;; essential when my files are lost. Though today, most people prefer some VC
  ;; systems to find manage their files of different versions.
  (setq make-backup-files t
        vc-make-backup-files t          ; even the file is managed by VC
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
        auto-save-include-big-deletions t)
  )

;; Customized hook/advice.
(use-package emacs
  :init

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

  )

;; Kill behavior.
(use-package simple
  :init
  ;; Do not add a string to kill-ring if it duplicates the last one, which make
  ;; the kill-ring easier to peruse (e.g. `consult-yank-pop').
  (setq kill-do-not-save-duplicates t)

  ;; Killing read-only texts only push it to the kill ring.
  (setq kill-read-only-ok t)
  (setq kill-transform-function
        (lambda (string) (and (not (string-blank-p string)) string)))

  ;; It seems that it's a bug from Emacs upstream, or at least,
  ;; `kill-whole-line' does not take `kill-transform-function' into
  ;; consideration.
  (define-advice kill-whole-line (:around (oldfun &rest args) no-kill-transform-fn)
    (let ((kill-transform-function))
      (apply oldfun args)))

  ;; If the data in the system clipboard is smaller than (in characters) this
  ;; number, then it's saved into the kill ring before a new entry is pushed into
  ;; the kill ring.
  (setq save-interprogram-paste-before-kill 200)
  )

(use-package simple
  :init
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package fill
  :init
  ;; Use a space to separate words of a different kind. Check pangu.js.
  (setq fill-separate-heterogeneous-words-with-space t
        ;; Prevent certain characters from being placed at the beginning or end
        ;; of a line by filling.
        enable-kinsoku t
        ;; Ending a sentence with two spaces is considered to be a historical
        ;; convention.
        sentence-end-double-space nil))

(use-package indent
  :init
  ;; TAB (`indent-for-tab-command') behavior. TAB can
  ;;   (a) indent current line if the cursor is within the beginning indentation,
  ;;   (b) insert literal whitespaces (tab or/and space).
  ;; In most major modes, TAB can insert spaces and also use spaces to indent
  ;; lines. In some major modes, such as `makefile-mode', it's important to
  ;; insert literal tab character, so these modes usually set `indent-tabs-mode'
  ;; to t.
  (setq-default tab-always-indent nil
                indent-tabs-mode nil
                ;; Distance between each tab stop. This option is useful when
                ;; `tab-stop-list' is set to nil (the default value).
                tab-width 4)
  )

(use-package prettify-symbols-mode
  :init
  ;; Show the non-prettified version of a symbol when point is on it.
  (setq prettify-symbols-unprettify-at-point t)
  )

(use-package crm
  :init
  ;; From minad/vertico.
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (define-advice completing-read-multiple (:filter-args (args) with-indicator)
    "Filter the first argument of `completing-read-multiple'."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  )

(use-package minibuffer
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; More minibuffer history.
  (setq history-length 1000)
  ;; Embark makes this option really meaningful!
  (setq enable-recursive-minibuffers t)
  ;; Restore window configurations on exit from minibuffer.
  (setq read-minibuffer-restore-windows t)
  )

(use-package recentf
  :hook ((after-init . recentf-mode)
         (kill-emacs . recentf-cleanup))
  :init
  (setq recentf-max-saved-items 240) ; just because I like this number
  (setq recentf-exclude '("\\.?cache" ".cask" "COMMIT_EDITMSG\\'"
                          "\\.\\(?:gz\\|gif\\|svg\\|png\\|PNG\\|jpe?g\\|JPE?G\\|bmp\\|xpm\\)$"
                          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                          "^/tmp/" "^/var/folders/.+$" "/persp-confs/" "HELLO"))
  :config
  ;; Auto-cleanup recentf if Emacs is run as daemon.
  (setq recentf-auto-cleanup (if (daemonp) 300))

  ;; REVIEW: Use this in lieu of `doom--recentf-file-truename-fn'.
  ;;   See emacs-mirror/emacs@32906819addd.
  ;; (setq recentf-show-abbreviated t)

  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))

  (defun recentf--file-truename-abbred-fn (file)
    "Get the truename of FILE and strip out the /sudo:X@ prefix."
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))

  ;; Filenames are shortened.
  (add-to-list 'recentf-filename-handlers #'recentf--file-truename-abbred-fn)
  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;; Save minibuffer history across sessions.
(use-package savehist
  :hook (after-init . savehist-mode)
  :init
  ;; Additional variables to save.
  (setq savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        ;; Time-based autosave of minibuffer history.
        savehist-autosave-interval 300)
  )

;; `save-place-mode': remember where I was when I last visit this file!
(use-package saveplace
  :hook (after-init . save-place-mode))

;; Typed text replaces the selection. Does not mean too much in `evil-mode'.
(use-package delsel
  :init
  (add-hook 'after-init-hook #'delete-selection-mode)
  (setq delete-selection-temporary-region nil))

(use-package tabify
  :init
  ;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
  ;; middle of a line.
  (setq tabify-regexp "^\t* [ \t]+"))

(use-package comp
  :init
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-jit-compilation t))

(use-package register
  :config
  (set-register ?m '(buffer . "*Messages*")))

(use-package elec-pair
  :init
  (add-hook 'after-init-hook #'electric-pair-mode))

(use-package isearch
  :config
  (setq isearch-lazy-count t))

(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode))

(use-package hideshow
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(use-package so-long
  :init
  (add-hook 'after-init-hook #'global-so-long-mode))

(use-package calendar
  :config
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))

(use-package webjump
  :config
  (add-to-list 'webjump-sites
               `("Google" .
                 [simple-query "www.google.com"
                               "www.google.com/search?q=" ""]))
  )

(use-package doc-view
  :config
  (when sys/mac
    ;; This is a wrapper script defined in my Nix configuration.
    (setq doc-view-odf->pdf-converter-program "soffice-cli")))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  ;; Reuse the path settings of your remote account when you log in.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package subword
  :diminish ((superword-mode . " 󱔎")
             (subword-mode . " 󱗀"))
  :init
  (defalias 'camel-case-mode 'subword-mode)
  (defalias 'snake-case-mode 'superword-mode))

(use-package server
  :init
  (defun safe-server-start ()
    (require 'server)         ; must required
    (unless (server-running-p)
      (server-start)))

  (add-hook 'after-init-hook #'safe-server-start))



(provide 'init-mini)
;;; init-mini.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
