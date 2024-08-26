;;; init-vterm.el -- Terminal emulator. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; In-Emacs terminal emulator.
(use-package vterm
  ;; Emacs-vterm needs to be dynamically linked to libvterm.
  :when (bound-and-true-p module-file-suffix)

  :init
  (celeste/prepare-package vterm)
  (autoload 'vterm-at "vterm-x" nil t)

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


  ;; `spawn-sub-emacs'
  (add-to-list 'vterm-keymap-exceptions "<f9>")

  ;; Mode-line for vterm buffer.
  (add-hook 'vterm-mode-hook
            (defun +vterm--simplify-mode-line ()
              (setq-local mode-line-format
                          '((ace-window-display-mode
                             (:eval
                              (window-parameter
                               (selected-window)
                               'ace-window-path)))
                            "%e" mode-line-front-space
                            " "
                            mode-line-modes
                            mode-line-misc-info
                            mode-line-end-spaces))))


  (require 'vterm-x)

  (use-package vterm-notify
    :init (celeste/prepare-package alert)
    :diminish " "
    ;; Same as iTerm2.
    :bind ("M-s-a" . vterm-notify-mode))

  )



(provide 'init-vterm)
;;; init-vterm.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
