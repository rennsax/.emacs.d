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

  )

;; `consult-flycheck'
(use-package consult-flycheck
  :after flycheck
  :init
  (celeste/prepare-package consult-flycheck)
  :bind (("C-c s d" . consult-flycheck)))



(provide 'init-flycheck)
;;; init-flycheck.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
