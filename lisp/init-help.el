;;; init-help.el -- Make Emacs manual more readable! -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Better Emacs *help* buffer that provides much more contextual information.
;; For example, source code, references, key bindings, ...
(use-package helpful
  :init
  ;; dependency
  (celeste/prepare-package s)
  (celeste/prepare-package dash)
  (celeste/prepare-package elisp-refs)
  (celeste/prepare-package helpful)

  :bind (([remap describe-function] . helpful-callable) ; `helpful-function' excludes macros
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("C-h h" . helpful-at-point)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-command] . helpful-command))
  :config
  (add-hook 'helpful-mode-hook #'visual-line-mode))

;; Show demos in the *Help* or `helpful-mode' buffer.
(use-package elisp-demos
  :init
  (celeste/prepare-package elisp-demos)
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
