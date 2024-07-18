;;; init-vterm.el -- Terminal emulator. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; In-Emacs terminal emulator.
(use-package vterm
  ;; Emacs-vterm needs to be dynamically linked to libvterm.
  :when (bound-and-true-p module-file-suffix)

  :commands vterm

  :init
  (celeste/prepare-package vterm)

  (defun vterm-at (file &optional arg)
    "Open a vterm buffer that is closest to FILE.

If FILE is a directory, open vterm at this directory. If FILE is a regular file,
open vterm at its parent directory."
    (interactive "fVterm directory: \nP")
    (unless (file-exists-p file)
      (error "FILE %s does not exist!" file))
    (unless (file-directory-p file)
      (setq file (file-name-directory file)))
    ;; Convert directory name to absolute, and remove the tailing slash.
    (setq file (directory-file-name (expand-file-name file)))
    (let* ((default-directory file)
           (vterm-buf-name
            (format "*vterm<%s>*" (file-name-base file)))
           (buf (get-buffer vterm-buf-name)))
      (if (and (not arg)
               buf
               (buffer-live-p buf)
               (file-equal-p
                (with-current-buffer buf default-directory)
                default-directory))
          (switch-to-buffer buf)
        ;; If a string is given, `vterm' will always open a new terminal.
        (vterm vterm-buf-name))))

  (with-eval-after-load 'project
    (defun project-vterm ()
      "Switch to the existing project vterm buffer or create a new one."
      (interactive)
      (vterm-at (project-root (project-current t))))
    (keymap-set project-prefix-map "t" #'project-vterm))

  (with-eval-after-load 'embark
    (keymap-set embark-file-map "t" #'vterm-at))

  (with-eval-after-load 'dired
    (defun dired-do-open-vterm (file)
      "Open vterm according to the current cursor position in the dired buffer."
      (interactive (list (dired-get-filename)))
      (vterm-at file))
    ;; Override `dired-do-touch'.
    (keymap-set dired-mode-map "T" #'dired-do-open-vterm))

  :bind (("C-c b t" . (lambda ()
                        (interactive)
                        (vterm-at "~"))))

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
