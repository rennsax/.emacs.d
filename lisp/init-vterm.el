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
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "t" #'project-vterm))

  (defun +vterm-at (file &optional arg)
    (interactive "fVterm directory: \nP")
    (while (not (file-directory-p file))
      (setq file (file-name-directory file)))
    (let ((default-directory file))
      ;; Use C-u prefix, so a new vterm is always created.
      (vterm (or arg '(4)))))

  (with-eval-after-load 'embark
    (keymap-set embark-file-map "t" #'+vterm-at))

  :commands vterm

  :bind (("C-c b t" . (lambda ()
                        (interactive)
                        (let ((default-directory "~"))
                          (vterm '(4))))))

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
             ("C-x C-e" . (lambda () (interactive)
                            (vterm-send-key "x" nil nil 'ctrl)
                            (vterm-send-key "e" nil nil 'ctrl)))
             ("C-q" . vterm-send-next-key)
             :map vterm-copy-mode-map
             ("q" . vterm-copy-mode-done)
             ("p" . previous-line)
             ("n" . next-line))

  ;; It's true that I'm a Zsh lover.
  (setq vterm-tramp-shells '(("docker" "/bin/sh")
                             ;; REVIEW: Why quoted? See https://github.com/akermu/emacs-libvterm/issues/569#issuecomment-1615431427
                             ;; It might be a Tramp bug.
                             ("ssh" "'/bin/zsh'")))
  )



(provide 'init-vterm)
;;; init-vterm.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
