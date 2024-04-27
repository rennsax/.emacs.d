;;; init-tool.el -- Tools integration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-package))

;; Edit anything, everywhere, w/ an popped Emacs frame!
(celeste/use-package emacs-everywhere
  :commands emacs-everywhere)

;; Yet another great "ripgrep" frontend!
(celeste/use-package spinner)
(celeste/use-package deadgrep
  ;; Fancy progress-bars in mode-line.
  :commands deadgrep)

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
  ;; That's because I'm using EVIL 😈.
  (setq persp-suppress-no-prefix-key-warning t)
  ;; Ignore some buffers with `persp-switch-to-buffer*'
  (setq ido-ignore-buffers `("\\` " "\\*helpful.*\\*"))
  (setq persp-state-default-file (concat celeste-data-dir "last-perspective"))

  (keymap-set global-map "C-x x" 'perspective-map)

  (add-to-list 'delete-frame-functions #'(lambda (_) (persp-state-save)))
  )

;; Enchanted spell checker.
(celeste/use-package jinx
  :commands jinx-mode
  :bind (:map jinx-overlay-map
              ("C-c j c" . jinx-correct))
  :config
  (setq jinx-languages "en_US")
  ;; Exclude Chinese characters. This should be a universal setting, so I put it
  ;; here (instead of init-cjk.el)
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))

  :init
  (defconst jinx-mode-dict-alist
    '((emacs-lisp-mode ("el"))
      (python-mode ("numpy")))
    "Mode-local dictionaries.")

  (defconst jinx-enable-mode-list
    '(emacs-lisp-mode org-mode)
    "Modes that jinx should be enabled.")

  ;; Manually setup mode-local words and enable jinx in specified modes.
  (mapc (lambda (mode-word-list)
          (let ((mode (car mode-word-list))
                (word-list (cadr mode-word-list)))
            (add-hook (intern (concat (symbol-name mode) "-hook"))
                      #'(lambda ()
                          (setq jinx-local-words
                                (mapconcat #'identity word-list " "))))))
        jinx-mode-dict-alist)
  ;; Must *after* setting `jinx-local-words'.
  (celeste/add-mode-hook jinx-enable-mode-list #'jinx-mode)
  )

;; In-Emacs terminal emulator.
(celeste/use-package vterm
  ;; TODO: tell whether current Emacs support dynamic-modules.
  :when (bound-and-true-p module-file-suffix)
  :commands vterm
  :config
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))

  ;; Kill the vterm immediately once vterm is dead.
  (setq vterm-kill-buffer-on-exit t)

  ;; Add a zero after the default value.
  (setq vterm-max-scrollback 10000)

  ;; The horizontal margin is useless.
  (add-hook 'vterm-mode-hook #'(lambda ()
                                 (setq-local hscroll-margin 0)
                                 (when (featurep 'evil-escape)
                                   (evil-escape-mode -1)))))

(celeste/use-package multi-vterm
  :commands (multi-vterm
             multi-vterm-dedicated-toggle
             multi-vterm-dedicated-open))

(celeste/use-package projectile
  ;; Global minor mode to enable projectile functionalities.
  :hook (after-init . projectile-mode)
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


(celeste/use-package mwim
  :commands mwim-beginning mwim-end)

(celeste/use-package elisp-demos
  :commands elisp-demos-advice-helpful-update elisp-demos-advice-describe-function-1
  :init
  ;; For traditional *Help*.
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  ;; For helpful.
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(provide 'init-tool)
;;; init-tool.el ends here
