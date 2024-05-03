;;; init-vc.el -- Version control tools. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; Magit

;; Transient is a builtin package for implementing keyboard-driven menus (mainly
;; used for Magit). However, we use the submodule one, because it's actively
;; developed, and newer version of magit may depend on it.
(add-to-list 'load-path (concat celeste-package-dir "transient/lisp"))
(with-eval-after-load 'transient
  ;; Set transient directories.
  (setq transient-levels-file  (concat celeste-data-dir "transient/levels")
        transient-values-file  (concat celeste-data-dir "transient/values")
        transient-history-file (concat celeste-data-dir "transient/history"))

  (setq transient-default-level 5
        ;; Always display the transient popup buffer below.
        transient-display-buffer-action '(display-buffer-below-selected))
  (keymap-set transient-map "<escape>" #'transient-quit-one))

;; Dependency: with-editor
(add-to-list 'load-path (concat celeste-package-dir "with-editor/lisp"))

;; Also depends on `compat' and `dash'.
;; TODO: `magit-submodule-add-1' git submodule absortgitdirs
(add-to-list 'load-path (concat celeste-package-dir "magit/lisp"))
(use-package magit
  :demand t ; The killer feature of Emacs! Load it immediately!
  :defines (magit-diff-refine-hunk
            magit-save-repository-buffers
            magit-mode-map
            magit-revision-show-gravatars
            magit-bury-buffer-function
            magit-display-buffer-function)
  :commands magit-mode-quit-window
  :bind (("C-c g g" . magit)
         ("C-c g e i" . magit-gitignore-in-topdir)
         ("C-c g m a" . magit-submodule-add)
         ("C-c g m d" . magit-submodule-remove)
         ("C-c g m c" . magit-clone))
  :config
  (setq magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Just trust the user, instead of saving files before running magit
        ;; commands.
        magit-save-repository-buffers nil)

  (keymap-set magit-mode-map "s-r" 'magit-refresh)
  (keymap-unset magit-mode-map celeste-leader-key)

  ;; Show gravatars when viewing revision.
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  ;; Magit window settings.
  (celeste/autoload '+magit-display-buffer-fn magit)
  (setq magit-bury-buffer-function #'magit-mode-quit-window
        magit-display-buffer-function #'+magit-display-buffer-fn)

  ;; Add missing options.
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; Clean up after magit by killing leftover magit buffers and reverting
  ;; affected buffers (or at least marking them as need-to-be-reverted).
  (celeste/autoload '+magit/quit magit)
  (celeste/autoload '+magit/quit-all magit)
  (bind-keys :map magit-mode-map
             ("q" . +magit/quit)
             ("Q" . +magit/quit-all))
  )


;;; diff-hl: better git-diff integration

(add-to-list 'load-path (concat celeste-package-dir "diff-hl"))

(use-package diff-hl
  ;; `find-file-hook': after a buffer is loaded from a file
  :hook ((find-file . diff-hl-mode)
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
  )

(use-package diff-hl-flydiff
  :hook (diff-hl-mode . diff-hl-flydiff-mode))
(use-package diff-hl-show-hunk
  :commands diff-hl-show-hunk)
(use-package diff-hl-dired
  :hook (dired-mode . diff-hl-dired-mode))


(provide 'init-vc)
;;; init-vc.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
