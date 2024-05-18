;;; init-vterm.el -- Terminal emulator. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; In-Emacs terminal emulator.
(use-package vterm
  ;; Emacs-vterm needs to be dynamically linked to libvterm.
  :when (bound-and-true-p module-file-suffix)
  :init
  (celeste/prepare-package vterm)
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
                                   (evil-escape-mode -1))))
  (bind-keys :map vterm-mode-map
             ("C-u" . vterm--self-insert)
             ;; TODO: `vterm--self-insert' seems has no effect.
             ("C-x C-e" . (lambda () (interactive)
                            (vterm-send-key "x" nil nil 'ctrl)
                            (vterm-send-key "e" nil nil 'ctrl))))
  )
;; Manage multiple vterm buffers.
(use-package multi-vterm
  :init
  (celeste/prepare-package multi-vterm)
  :bind (("C-c b t" . multi-vterm))
  :commands (multi-vterm-dedicated-toggle
             multi-vterm-dedicated-open))

;;;###autoload
(defun +project-vterm ()
  "Open vterm at the project root."
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (if current-prefix-arg
        (multi-vterm)
      (vterm))))

(when (bound-and-true-p project-switch-commands)
  (add-to-list 'project-switch-commands
               '(+project-vterm "vterm" "t")))



(provide 'init-vterm)
;;; init-vterm.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
