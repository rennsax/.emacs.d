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


;;; Basic keybindings.
;; NOTE: Package-specified bindings should be put into their corresponding
;; sections (their `use-package' configurations, for example).

(bind-keys ("M-J" . join-line)
           ("C-S-n" . scroll-up-line)
           ("C-S-p" . scroll-down-line)
           :map prog-mode-map
           ("M-RET" . comment-indent-new-line))

;; Increasing or decreasing the default font size in all GUI Emacs
;; frames. (new feature in Emacs 29.1)
;; TODO: reset with "s-0"
(bind-keys ("s-+" . (lambda () (interactive) (global-text-scale-adjust 1)))
           ("s-_" . (lambda () (interactive) (global-text-scale-adjust -1))))

;; Map the right command to control. Mac's right command key is redundant to me.
(when sys/mac
  (setq mac-right-command-modifier 'control))


;;; Extra settings.

;; Always keeps my "remote" files on the OrbStack VM. By default, They become unreachable
;; when I turn down the VM, and are removed from the recentf list.
(with-eval-after-load 'recentf
  (add-to-list 'recentf-keep "^/ssh:orb:"))

;; Also consider `superword-mode'.
(add-hook 'prog-mode-hook #'subword-mode)


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


;;; More configurations for builtin packages.

(use-package register
  :config
  (set-register ?m '(buffer . "*Messages*")))

(use-package project
  :init
  (setq project-switch-commands '((project-find-file "Find file")
                                  (consult-fd "Fd" "F")
                                  (consult-ripgrep "Ripgrep" "R")
                                  (deadgrep "Deadgrep" "G")
                                  (project-dired "Dired")
                                  (magit-project-status "Magit" "m")
                                  (project-eshell "Eshell")
                                  (keyboard-quit "Quit" "q")))
  :config
  (dolist (unmap-key '("s" "v"))
    (keymap-unset project-prefix-map unmap-key t)))

(use-package tab-bar
  :custom (tab-bar-select-tab-modifiers '(super)) ; use "s-[0-9]" to switch tabs
  :config
  (setq tab-bar-show 1                 ; hide tab bar when there is only one tab
        tab-bar-close-button-show nil  ; hide the ugly close button
        tab-bar-tab-hints t            ; show number
        tab-bar-new-tab-choice #'get-scratch-buffer-create)

  (delq 'tab-bar-format-add-tab tab-bar-format))

;; Restore old window configurations.
(use-package winner
  :hook (after-init . winner-mode)
  :bind (:map window-prefix-map
              ("C-/" . winner-undo)
              ("C-?" . winner-redo)))
(use-package man
  :init
  (when sys/mac
    ;; Check: https://github.com/abo-abo/swiper/issues/2836#issuecomment-831292443.
    ;; 1. Install man-db (nongnu's implementation).
    ;; 2. Run `mandb' in the shell to build cache.
    (setq manual-program "gman")))


;;; Minibuffer tweaks.

;; Embark makes this option really meaningful!
(setq enable-recursive-minibuffers t)

(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode))

;; Restore window configurations on exit from minibuffer.
(setq read-minibuffer-restore-windows t)


;;; Bootstrap custom file.
;;; NOTE: must be here now.

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  ;; `load' is more primitive than `load-file'.
  (load custom-file nil nil t))


(celeste/add-mode-hook '(prog-mode text-mode)
    (defun +set-show-trailing-whitespace ()
      (setq-local show-trailing-whitespace t)))


;;; Handle line wrap in different modes.

;; HACK: the "*Message*" buffer has been created before, so `add-hook' to
;; `message-mode-hook' does not help.
(with-current-buffer "*Messages*" (visual-line-mode))

;; Celeste Emacs has presets for modes in which `visual-line-mode' and
;; `auto-fill-mode' should be enabled.
(celeste/add-mode-hook celeste-visual-line-mode-list #'visual-line-mode)
(celeste/add-mode-hook celeste-auto-fill-mode-list #'celeste/auto-fill-mode)

(define-minor-mode celeste/auto-fill-mode
  "Toggle `auto-fill-mode' and `display-fill-column-indicator-mode' together."
  :init-value nil
  :global nil
  (if celeste/auto-fill-mode
      (progn
        (auto-fill-mode +1) (display-fill-column-indicator-mode +1))
    (auto-fill-mode -1)
    (display-fill-column-indicator-mode -1)))


;;; Compilation

;; Emacs provides two building facilities: byte-compilation and
;; native-compilation. The differences are the results: byte codes (.elc) or
;; native instructions (.eln).

;; If Emacs is built with native-compilation enabled, then Emacs will try to
;; compile *.elc files to *.eln files asynchronously. However, many third-party
;; packages can not be native-compiled inherently. As a compromise, I enable the
;; native-compilation feature when building Emacs (so the builtin packages can
;; be natively compiled) and disable the `native-comp-jit-compilation' options
;; (so I can byte compile other elisp codes w/o automatically trigger Emacs's
;; async compilation).

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-jit-compilation nil)  ; do not auto build .elc to .eln


(provide 'init-mini-extra)
;;; init-mini-extra.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
