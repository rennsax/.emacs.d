;;; init-vc.el -- Version control tools. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; Magit

(celeste/prepare-package compat)

;; Magit, forge, transient, etc., are developed by the same author (@tarsius,
;; Jonas Bernoulli). His packages are common to startup. Assume that we are at
;; the root directory of Magit, then typically we need to:
;; 1. make lisp
;; 2. make info
;; Then, the info doc will appear in the "docs" subdirectory, and .elc files
;; appear in the "lisp" subdirectory. Add the previous to
;; `Info-default-directory-list' and add the latter to `load-path'.

;; Transient is a builtin package for implementing keyboard-driven menus (mainly
;; used for Magit). However, we use the submodule one, because it's actively
;; developed, and newer version of magit may depend on it.
(use-package transient
  :init
  (celeste/prepare-package-2 transient "lisp" :info "docs")

  :config
  ;; Set transient directories.
  (setq transient-levels-file  (celeste/make-path "transient/levels.el" 'state)
        transient-values-file  (celeste/make-path "transient/values.el" 'state)
        transient-history-file (celeste/make-path "transient/history.el" 'state))

  (setq transient-default-level 5
        ;; Always display the transient popup buffer below.
        transient-display-buffer-action '(display-buffer-below-selected))
  (keymap-set transient-map "<escape>" #'transient-quit-one))

(use-package with-editor
  :init
  (celeste/prepare-package-2 with-editor "lisp" :info "docs")
  :hook ((vterm-mode . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor))
  :bind (("<remap> <shell-command>" . with-editor-shell-command)
         ("<remap> <async-shell-command>" . with-editor-async-shell-command))
  :autoload with-editor
  :config
  (setq with-editor-cancel-query-functions
        (list
         (lambda (force) (or force (y-or-n-p "Cancel with-editor?")))))
  )

(use-package magit
  :init
  (celeste/prepare-package-2
      (dash (magit "lisp" :info "docs")))

  :commands magit-mode-quit-window
  :bind (("C-c g g" . magit)
         ("C-c g e i" . magit-gitignore-in-topdir)
         ("C-c g m a" . magit-submodule-add)
         ("C-c g m d" . magit-submodule-remove)
         ("C-c g m c" . magit-clone)
         ("C-c g f s" . magit-stage-buffer-file)
         ("C-c g c" . magit-commit))
  :hook (after-init . magit-auto-revert-mode)
  :config
  (setq magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Just trust the user, instead of saving files before running magit
        ;; commands.
        magit-save-repository-buffers nil)

  (keymap-set magit-mode-map "s-r" 'magit-refresh)

  ;; Show gravatars when viewing revision.
  ;; REVIEW: removed, because it's slow.
  ; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  ;; Magit window settings.
  (defun +magit-display-buffer-fn (buffer)
    "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ((and (eq buffer-mode 'magit-status-mode)
                     (get-buffer-window buffer))
                '(display-buffer-reuse-window))
               ;; Any magit buffers opened from a commit window should open below
               ;; it. Also open magit process windows below.
               ((or (bound-and-true-p git-commit-mode)
                    (eq buffer-mode 'magit-process-mode))
                (let ((size (if (eq buffer-mode 'magit-process-mode)
                                0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))

               ;; Everything else should reuse the current window.
               ((or (not (derived-mode-p 'magit-mode))
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode))))
                '(display-buffer-same-window))))))

  ;; So it can be successfully advised by `init-window'
  (setq magit-display-buffer-function #'+magit-display-buffer-fn)

  (setq magit-bury-buffer-function #'magit-mode-quit-window)

  ;; Add missing options.
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (transient-append-suffix 'magit-dispatch "r"
    '("R" "Rename" magit-file-rename))

  ;; Hybrid of `magit-checkout' and `magit-branch-and-checkout'.
  (transient-replace-suffix 'magit-branch 'magit-checkout
    '("b" "dwim" magit-branch-or-checkout))

  )

;; Show TODOs in magit buffer.
(use-package magit-todos
  :after (magit hl-todo)
  :demand t
  :init
  (celeste/prepare-package (async dash f s pcre2el magit-todos))
  :config
  (magit-todos-mode +1))



;;; git-modes: collection of git{ignore,config,attributes}-mode'.

;; (celeste/package-build-autoload 'git-modes)
(celeste/package-autoload 'git-modes)


;;; Forge
;; Interact with Github, GitLab in Emacs.

;;; forge

;; https://magit.vc/manual/ghub/Storing-a-Token.html
;; PR: https://www.youtube.com/watch?v=Qj4eRccsos8
(use-package forge
  :after magit

  :init
  ;; All these sort of things are dependencies of forge (incredible!)
  (celeste/prepare-package-2
      (emacsql closql yaml treepy
               (ghub "lisp" :info "docs") (forge "lisp" :info "docs")))
  (unless (load "forge-autoloads" t t)
    (warn "Cannot load `forge-autoloads'. Maybe you forget to run ‘make lisp’ for forge?"))

  (setq forge-database-file (celeste/make-path "forge-db.sqlite" 'state))

  :commands forge-pull
  :config
  (defalias 'forge-issue-close 'forge-issue-state-set-completed "Close the issue.")

  ;; A network error after submitting post makes me lose the post. So I add an
  ;; advice to backup the recent submitted post.
  (advice-add #'forge-post-submit :before
              (defun +forge-backup-buffer-before-submit (&rest _)
                (save-buffer)
                (copy-file buffer-file-name "/tmp/forge-post-backup" t)))
  )


;;; diff-hl: better git-diff integration

(use-package diff-hl
  :init
  (celeste/prepare-package diff-hl)

  ;; Seeing VC gutter while writing docs is just .. distracted.
  :hook ((prog-mode . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; `diff-hl-update' will use `make-thread' to create a new thread and update
  ;; diff info asynchronously.
  ;; (setq diff-hl-update-async t)
  ;; REVIEW: I found the async way may have some bugs, for example a) slow, even
  ;; slower than mono-thread, b) `save-buffer' sometimes suck, I need to press
  ;; the shortcut for multiple times.

  ;; Do not show stage changes. This is similar to most of prevalent editors.
  (setq diff-hl-show-staged-changes nil)

  (define-advice diff-hl-show-hunk (:after (&rest _) kill-diff-buffer)
    (kill-buffer diff-hl-show-hunk-buffer-name))
  )

(use-package diff-hl-flydiff
  :hook (diff-hl-mode . diff-hl-flydiff-mode))
(use-package diff-hl-show-hunk
  :commands diff-hl-show-hunk)
(use-package diff-hl-dired
  :commands diff-hl-dired-mode
  :init
  (add-hook 'dired-mode-hook
            (lambda () (unless (file-remote-p default-directory)
                    (diff-hl-dired-mode)))))


(provide 'init-vc)
;;; init-vc.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
