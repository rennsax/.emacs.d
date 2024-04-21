;;; init-builtin.el -- Configurations for awesome builtin tools. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-package))

(require 'init-custom)

;;; Editor basis
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
  :hook (after-init . size-indication-mode)
  :config

  ;; Show column number at the modeline.
  (setq column-number-mode t
        line-number-mode nil)

  (celeste/add-mode-hook '(prog-mode markdown-mode conf-mode)
      (defun enable-trailing-whitespace ()
        "Show trailing spaces and delete on saving."
        (setq show-trailing-whitespace t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local)))
  (celeste/add-mode-hook celeste-visual-line-mode-list #'visual-line-mode)
  (celeste/add-mode-hook celeste-auto-fill-mode-list #'auto-fill-mode)

  ;; HACK: the "*Message*" buffer has been created before, so `add-hook' to
  ;; `message-mode-hook' does not help.
  (with-current-buffer "*Messages*"
    (visual-line-mode)))

(use-package tramp
  :config
  ;; Also backup remote file locally
  (setq tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory  (concat celeste-cache-dir "tramp-autosave/")))

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(use-package tabify
  :config
  (setq tabify-regexp "^\t* [ \t]+"))

;;; Emacs shell - wow, such a versatile and powerful shell, seamlessly
;;; integrated with Emacs itself! 🐚
(use-package eshell
  :defines (eshell-scroll-to-bottom-on-input
            eshell-history-size
            eshell-prompt-function
            eshell-mode-map)

  :config

  (setq eshell-scroll-to-bottom-on-input t)
  ;; More history!
  (setq eshell-history-size 10000)

  (load (concat celeste-autoload-dir "eshell-prompt"))
  (setq eshell-prompt-function '+eshell-default-prompt-fn)

  (celeste/autoload '+eshell-input-bol eshell)
  (celeste/autoload '+eshell-kill-whole-input eshell))

;; To most programs, eshell is dumb terminal. Therefore we need to tell Eshell
;; to open up visual commands in a dedicated terminal emulator.
(use-package em-term
  :config
  (setq eshell-visual-subcommands
        '(("git" "log" "ls" "diff" "difftool" "show")))
  (add-to-list 'eshell-visual-commands "nvim"))


;;; Emacs dired - practical file explorer 📂
(use-package dired
  ;; TODO
  ;; :bind (:map dired-mode-map
  ;;        ("C-c C-p" . wdired-change-to-wdired-mode))
  ;; :defines ls-lisp-use-insert-directory-program
  :config
  ;; Use the dir in another dired window as the default dir rather than the
  ;; current one.
  (setq dired-dwim-target t)
  ;; Copies and deletes are recursive, of course.
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; On macOS, try to use GNU ls. If not found, use its own ls instead, and
  ;; modify `dired-listing-switches' for compatibility.
  (when sys/macp
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-alh")))))

;; Extra functionality for dired.
;; Open file with system utilities, hide files, etc.
(use-package dired-x
  :config
  (let ((cmd (cond (sys/mac-x-p "open")
                   (sys/linux-x-p "xdg-open")
                   (sys/win32p "start")
                   (t ""))))
    ;; Rules for suggested commands.
    ;; Take precedence over `dired-guess-shell-alist-default'.
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(use-package gamegrid
  :init
  (setq gamegrid-user-score-file-directory (concat celeste-data-dir "games")))


(provide 'init-builtin)
;;; init-builtin.el ends here
