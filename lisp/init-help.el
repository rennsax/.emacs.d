;;; init-help.el -- Make Emacs manual more readable! -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Better Emacs *help* buffer that provides much more contextual information.
;; For example, source code, references, key bindings, ...
(celeste/use-package helpful
  :init
  ;; dependency
  (celeste/use-package elisp-refs)

  :bind (("C-h f" . helpful-callable) ; `describe-function'
         ("C-h v" . helpful-variable) ; `describe-variable'
         ("C-h k" . helpful-key)      ; `describe-key'
         ("C-h h" . helpful-at-point)
         ("C-h x" . helpful-command))  ; `describe-command'
  :config
  (add-hook 'helpful-mode-hook #'visual-line-mode))

;; Show demos in the *Help* or `helpful-mode' buffer.
(celeste/use-package elisp-demos
  :commands elisp-demos-advice-helpful-update elisp-demos-advice-describe-function-1
  :init
  ;; For traditional *Help*.
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  ;; For helpful.
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))


(provide 'init-help)
;;; init-help.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
