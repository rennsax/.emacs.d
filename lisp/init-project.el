;;; init-project.el -- Project support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package project
  :init
  (setq project-switch-commands '((project-find-file "Find file")
                                  (consult-fd "Fd" "F")
                                  (consult-ripgrep "Ripgrep" "R")
                                  (deadgrep "deadGrep" "G")
                                  (project-dired "Dired")
                                  (magit-project-status "Magit" "m")
                                  (project-eshell "Eshell")
                                  (project-vterm "vTerm" "t")
                                  (keyboard-quit "Quit" "q")))
  :config
  (dolist (unmap-key '("s" "v"))
    (keymap-unset project-prefix-map unmap-key t)))


(use-package project-x
  :init (celeste/prepare-package project-x)
  :hook (after-init . project-x-mode) ; When Emacs is killed, auto save state!
  :config

  ;; Do not auto save project state.
  (setq project-x-save-interval nil)
  (setq project-x-window-list-file (celeste/make-path "project-x-window-list" 'state))
  (setq project-x-local-identifier
        '("mix.exs" ".project"))
  )


(defun project-kill-buffer-file-relative-path ()
  "Kill the relative path of current file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not bound to any file!"))
  (if-let* ((proj (project-current))
            (relative-path (file-relative-name buffer-file-name (project-root proj))))
      (progn
        (kill-new relative-path)
        (message relative-path))
    (user-error "Cannot get the current project!")))

(keymap-set project-prefix-map "C-w" #'project-kill-buffer-file-relative-path)



(provide 'init-project)
;;; init-project.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
