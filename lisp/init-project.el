;;; init-project.el -- Project management. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; projectile and perspective

;; Powerful and convenient *workspace* manager.
(celeste/use-package perspective
  ;; Differences: these two commands respect `ido-ignore-buffers'.
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :hook
  ((after-init . persp-mode)
   (kill-emacs . persp-state-save))
  :config
  ;; I don't set `persp-mode-prefix-key' - I manually map `perspective-map'.
  ;; That's because I used to be an EVIL user 😈.
  (setq persp-suppress-no-prefix-key-warning t)
  ;; Ignore some buffers with `persp-switch-to-buffer*'
  (setq ido-ignore-buffers `("\\` " "\\*helpful.*\\*"))
  (setq persp-state-default-file (concat celeste-data-dir "last-perspective"))

  (keymap-set global-map "C-c p" 'perspective-map)

  (add-to-list 'delete-frame-functions #'(lambda (_) (persp-state-save)))
  )

(celeste/use-package projectile
  ;; Global minor mode to enable projectile functionalities.
  :hook (after-init . projectile-mode)
  :bind (("C-c P s" . projectile-switch-project)
         ("C-c P f" . projectile-find-file)
         ("C-c P c" . projectile-compile-project)
         ("C-c P ." . projectile-recentf))
  :diminish
  :init
  (setq projectile-cache-file (concat celeste-cache-dir "projectile.cache")
        projectile-known-projects-file (concat celeste-data-dir "projectile-bookmarks.eld"))
  :commands projectile-dired
  :autoload project-projectile
  :config
  ;; Integration with project.el: use projectile's use Projectile’s project
  ;; lookup function (projectile-project-root) and project file lookup function
  ;; (projectile-project-files) whenever projectile-mode is enabled. Why?
  ;; Because many packages (eglot, deadgrep, etc.) support natively only
  ;; project.el's APIs.
  (add-hook 'project-find-functions #'project-projectile)
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'alien
        ;; If the cache is stale enough, clean with `projectile-invalidate-cache'.
        projectile-enable-caching t
        ;; After 10 minutes, the remote file existence cache expires.
        projectile-file-exists-remote-cache-expire (* 10 60)
        ;; Use # as the comment prefix for dirconfig (".projectile" by default).
        projectile-dirconfig-comment-prefix "#"
        ;; Run dired after switch to a project.
        projectile-switch-project-action #'projectile-dired
        ;; Keep the current project when switching projects, but put it at the end.
        projectile-current-project-on-switch 'move-to-end)
  )

;; Bridge projectile and perspective: when switching to a new project, create a
;; clean perspective.
(celeste/use-package persp-projectile
  :after perspective projectile
  :demand t)


(provide 'init-project)
;;; init-project.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
