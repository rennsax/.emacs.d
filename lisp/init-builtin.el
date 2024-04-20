;;; init-builtin.el -- Configurations for awesome builtin tools. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const))

;;; Emacs shell - wow, such a versatile and powerful shell, seamlessly
;;; integrated with Emacs itself! 🐚
(use-package eshell
  :config

  (setq eshell-scroll-to-bottom-on-input t)
  ;; More history!
  (setq eshell-history-size 10000)

  (load (concat celeste-autoload-dir "eshell-prompt"))
  (setq eshell-prompt-function '+eshell-default-prompt-fn)

  (celeste/autoload '+eshell-input-bol eshell)
  (celeste/autoload '+eshell-kill-whole-input eshell)

  (defun +eshell-setup-keys ()
    ;; TODO Shell-like C-d
    (evil-define-key* 'insert eshell-mode-map
      (kbd "C-p") #'eshell-previous-input
      (kbd "C-n") #'eshell-next-input
      (kbd "C-u") #'+eshell-kill-whole-input
      (kbd "C-a") #'+eshell-input-bol))

  (add-hook 'eshell-first-time-mode-hook '+eshell-setup-keys)

  ;; To most programs, eshell is dumb terminal. Therefore we need to tell Eshell
  ;; to open up visual commands in a dedicated terminal emulator.
  (use-package em-term
    :config
    (setq eshell-visual-subcommands
          '(("git" "log" "ls" "diff" "difftool" "show")))
    (add-to-list 'eshell-visual-commands "nvim"))

  )

;;; Emacs dired - practical file explorer 📂
(use-package dired
  ;; TODO
  ;; :bind (:map dired-mode-map
  ;;        ("C-c C-p" . wdired-change-to-wdired-mode))
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

;; Extra functionality (builtin)
;; Open file with system utilities, hide files, etc.
(use-package dired-x
  :demand t
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

;; Oh, the venilla dired is boring! I want more colors.
(celeste/use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Icons are also important!
(celeste/use-package nerd-icons-dired
  :diminish
  :custom-face
  ;; TODO
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))


(provide 'init-builtin)
;;; init-builtin.el ends here
