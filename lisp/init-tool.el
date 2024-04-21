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
  )


(provide 'init-tool)
;;; init-tool.el ends here
