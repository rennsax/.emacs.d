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
  (add-hook 'vterm-mode-hook (lambda () (setq-local hscroll-margin 0)))

  ;; Fix: in `vterm-copy-mode', "C-w" or "M-w" cannot correctly copy the active
  ;; region sometimes. Just reuse the logic of `vterm-copy-mode-done' but do not
  ;; exit `vterm-copy-mode'.
  ;;
  ;; Bind this to "C-w" and "M-w".
  (defun vterm-copy-mode-done-no-exit (arg)
    "Save the active region or line to the kill ring but do not exit `vterm-copy-mode'.

If a region is defined then that region is killed, with no region then
current line is killed from start to end.

The option `vterm-copy-exclude-prompt' controls if the prompt
should be included in a line copy.  Using the universal prefix ARG
will invert `vterm-copy-exclude-prompt' for that call."
    (interactive "P")
    (unless vterm-copy-mode
      (user-error "This command is effective only in vterm-copy-mode"))
    (unless (use-region-p)
      (goto-char (vterm--get-beginning-of-line))
      ;; Are we excluding the prompt?
      (if (or (and vterm-copy-exclude-prompt (not arg))
              (and (not vterm-copy-exclude-prompt) arg))
          (goto-char (max (or (vterm--get-prompt-point) 0)
                          (vterm--get-beginning-of-line))))
      (set-mark (point))
      (goto-char (vterm--get-end-of-line)))
    (kill-ring-save (region-beginning) (region-end)))

  (bind-keys :map vterm-mode-map
             ("C-u" . vterm--self-insert)
             ("C-x C-e" . (lambda () (interactive)
                            (vterm-send-key "x" nil nil 'ctrl)
                            (vterm-send-key "e" nil nil 'ctrl)))
             ("C-q" . vterm-send-next-key)
             :map vterm-copy-mode-map
             ;; RET: `vterm-copy-mode-done', which will copy the region or the current line.
             ("q" . vterm-copy-mode)
             ("p" . previous-line)
             ("n" . next-line)
             ("M-w" . vterm-copy-mode-done-no-exit)
             ("C-w" . vterm-copy-mode-done-no-exit))

  ;; Make tmux work at the remote shell. https://github.com/akermu/emacs-libvterm/issues/569.
  (define-advice vterm--get-shell (:filter-return (vterm-shell) quote-remote)
    "Quote VTERM-SHELL if it's a remote shell."
    (if (and (ignore-errors (file-remote-p default-directory))
             (not (string-match-p "'.*'" vterm-shell)))
        (format "'%s'" vterm-shell)
      vterm-shell))

  ;; It's the terminal emulator's duty to set COLORTERM env. Just like iTerm2 does:
  ;; https://gitlab.com/gnachman/iterm2/-/commit/978e1ab1b2ac7847d96dbabec808dfe767d45184.
  ;; See also neovim/libvterm:vterm.c, ls(1).
  (setq vterm-environment '("COLORTERM=truecolor"
                            ;; Unset PAGER variable.
                            ;; Tramp sets PAGER=cat via `tramp-remote-process-environment'.
                            "PAGER"))

  ;; `spawn-sub-emacs'
  (add-to-list 'vterm-keymap-exceptions "<f9>")
  )



(provide 'init-vterm)
;;; init-vterm.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
