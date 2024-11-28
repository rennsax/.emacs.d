;;; init-flycheck.el -- Flycheck setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Syntax checker.

;; flycheck: successor of the builtin flymake.
(use-package flycheck
  :init
  (celeste/prepare-package flycheck)

  ;; Enable syntax checking whenever eglot is managing the buffer.
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'flycheck-mode))

  :commands flycheck-mode
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

  ;; Default is 400. If more than threshold, an annoying warning appears.
  (setq flycheck-checker-error-threshold 1000)

  (defun flycheck-buffer-with (checker)
    "Use CHECKER to check the buffer."
    (interactive
     (if current-prefix-arg
         (list nil)
       (list (flycheck-read-checker "Select checker: "
                                    (flycheck-get-checker-for-buffer)))))
    (let ((flycheck-checker checker))
      (flycheck-buffer)))

  (keymap-set flycheck-command-map "S" #'flycheck-buffer-with)

  ;; Do not auto-enable `flycheck-mode' when edited org block.
  (with-eval-after-load 'org
    (define-advice org-edit-special (:after (&rest _) disable-flycheck)
      (flycheck-mode -1)))
  )

;; `consult-flycheck'
(use-package consult-flycheck
  :after flycheck
  :init
  (celeste/prepare-package consult-flycheck)
  :bind (("C-c s d" . consult-flycheck)))


;; Use `sideline' to show flycheck errors, just like VS Code's ErrorLen
;; extension.

(use-package sideline
  :init (celeste/prepare-package (ht sideline))
  :commands sideline-mode
  :diminish
  :config
  (setq sideline-backends-left-skip-current-line t
        ;; Allow right sideline at the current line
        sideline-backends-right-skip-current-line nil
        sideline-order-right 'down
        sideline-order-left 'down
        sideline-priority 100 ; overlays' priority
        sideline-delay 1.0 ; longer delay
        sideline-force-display-if-exceeds t
        )
  (setq sideline-backends-left '(sideline-flycheck)
        sideline-backends-right nil)
  ;; More eye-catching annotation.
  (setq sideline-format-left "%s   "
        sideline-format-right "   %s"
        sideline-display-backend-name t
        sideline-display-backend-format "[%s]"))

(use-package sideline-flycheck
  :init (celeste/prepare-package sideline-flycheck)
  :hook ((flycheck-mode . sideline-mode)
         (flycheck-mode . sideline-flycheck-setup))
  :config
  (setq flycheck-display-errors-function
        (defun flycheck-display-error-messages-unless-error-list-and-no-sideline (errors)
          "If `sideline-mode' is on, do not show errors."
          (unless (bound-and-true-p sideline-mode)
            (flycheck-display-error-messages-unless-error-list errors)))))


(provide 'init-flycheck)
;;; init-flycheck.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
