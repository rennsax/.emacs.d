;;; init-tool.el -- Tools integration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Edit anything, everywhere, w/ an popped Emacs frame!
(celeste/use-package emacs-everywhere
  :commands emacs-everywhere)

;; Yet another great "ripgrep" frontend!
(celeste/use-package deadgrep
  ;; Fancy progress-bars in mode-line.
  :init (celeste/use-package spinner)
  :commands deadgrep
  :config
  ;; (evil-set-initial-state 'deadgrep-mode 'motion)
  ;; Move across matched files.
  (evil-define-key* 'normal deadgrep-mode-map
    (kbd "C-P") #'deadgrep-backward-filename
    (kbd "C-N") #'deadgrep-forward-filename))


(provide 'init-tool)
;;; init-tool.el ends here
