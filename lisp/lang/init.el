;;; -*- lexical-binding: t; no-byte-compile: t; -*-

(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

(celeste/use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  ;; Check syntax when: the file is saved, a short time
  ;; (`flycheck-idle-change-delay') after the last change, and immediately after
  ;; `flycheck-mode' is enabled.
  ;; Of course, use `flycheck-buffer' to manually check the syntax.
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  ;; My load-path
  (setq flycheck-emacs-lisp-load-path 'inherit)
  )

;; Show diagnostic information in the buffer.
(celeste/use-package sideline
  :init
  ;; The missing hash table library for Emacs.
  (celeste/use-package ht)

  ;; :commands sideline-mode
  :config
  (setq sideline-backends-left-skip-current-line t
        ;; Allow right sideline at the current line
        sideline-backends-right-skip-current-line nil
        sideline-order-right 'down
        sideline-order-left 'down
        sideline-priority 100 ; overlays' priority
        sideline-delay 1.0 ; longer delay
        )
  ;; More eye-catching annotation.
  (setq sideline-format-left "⚠️%s⚠️  "
        sideline-format-right "   ⚠️%s⚠️"
        sideline-display-backend-name t
        sideline-display-backend-format "[%s]")

  ;; TODO sideline is a pretty package, but it's too young, and I've discovered
  ;; some bugs:
  ;; 1. `sideline-delay' is not respected
  ;; 2. when sideline is show, then `revert-buffer', the sideline will freeze
  ;; there.
  )

(celeste/use-package sideline-flycheck
  :hook ((flycheck-mode . sideline-mode)
         (flycheck-mode . sideline-flycheck-setup))
  :config
  ;; (setq sideline-backends-right '(sideline-flycheck))
  (setq sideline-backends-left '(sideline-flycheck))
  )

;; Many language tools respect envs. In Emacs (GUI), we need to fetch envs from
;; the shell.
(celeste/use-package exec-path-from-shell
  ;; :when (memq window-system '(mac ns))
  :commands exec-path-from-shell-copy-envs
  :init
  (defun +getenv-shell (variable &optional frame always)
    "Get the environment variable VARIABLE from shell.

VARIABLE and FRAME is passed to `getenv'.

If ALWAYS is non-nil, always try to copy env from shell.
Otherwise, if `getenv' returns non-nil, the result is returned
immediately."
    (if always
        (cdar (exec-path-from-shell-copy-envs (list variable)))
      (or (getenv variable frame)
          (cdar (exec-path-from-shell-copy-envs (list variable))))))
  )

(require 'init-yaml)
(require 'init-go)
(require 'init-markdown)
(require 'init-lua)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
