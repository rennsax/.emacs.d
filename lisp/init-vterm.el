;;; init-vterm.el -- Terminal emulator. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; In-Emacs terminal emulator.
(use-package vterm
  ;; Emacs-vterm needs to be dynamically linked to libvterm.
  :when (bound-and-true-p module-file-suffix)

  :init
  (celeste/prepare-package vterm)

  (defun project-vterm ()
    "Switch to the existing project vterm buffer or create a new one."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (project-name
            (file-name-nondirectory (directory-file-name default-directory)))
           (buf-name (format "*vterm<%s>*" project-name))
           (buf (get-buffer buf-name)))
      (if (and buf
               (buffer-live-p buf))
          (switch-to-buffer buf)
        (vterm buf-name))))

  :bind (("C-c b t" . vterm)
         :map project-prefix-map
         ("t" . project-vterm))

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
                            (vterm-send-key "e" nil nil 'ctrl)))
             :map vterm-copy-mode-map
             ("q" . vterm-copy-mode-done)
             ("p" . previous-line)
             ("n" . next-line))

  )



(provide 'init-vterm)
;;; init-vterm.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
