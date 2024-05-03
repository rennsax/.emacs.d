;;; init-tool.el -- Tools integration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Yet another great "ripgrep" frontend!
(celeste/use-package spinner)
(celeste/use-package deadgrep
  ;; Fancy progress-bars in mode-line.
  :bind ("C-c r g" . deadgrep))

;; Enchanted spell checker.
(celeste/use-package jinx
  :diminish
  :commands jinx-mode
  :bind (:map jinx-overlay-map
              ("C-c j c" . jinx-correct))
  :config
  (setq jinx-languages "en_US")
  ;; Exclude Chinese characters. This should be a universal setting, so I put it
  ;; here (instead of init-cjk.el)
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))

  :init
  (defcustom jinx-mode-dict-alist
    '((emacs-lisp-mode ("el"))
      (python-mode ("numpy"))
      (go-mode ("fmt")))
    "Mode-local dictionaries."
    :group 'jinx
    ;; TODO type?
    :type '(list (symbol (list string))))

  (defcustom jinx-enable-mode-list
    '(prog-mode text-mode)
    "Modes that jinx should be enabled."
    :group 'jinx
    :type '(list (symbol)))

  ;; Manually setup mode-local words and enable jinx in specified modes.
  (mapc (lambda (mode-word-list)
          (let ((mode (car mode-word-list))
                (word-list (cadr mode-word-list)))
            (add-hook (intern (concat (symbol-name mode) "-hook"))
                      #'(lambda ()
                          (setq jinx-local-words
                                (mapconcat #'identity word-list " "))
                          (jinx-mode +1)))))
        jinx-mode-dict-alist)
  ;; Must *after* setting `jinx-local-words'.
  (celeste/add-mode-hook jinx-enable-mode-list #'jinx-mode)
  )

;; Use "C-;" to ease your life!
(celeste/use-package embark
  :bind (("C-;" . embark-act))
  :commands embark-prefix-help-command
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(celeste/use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end))
  :commands mwim-beginning mwim-end)

(celeste/use-package elisp-demos
  :commands elisp-demos-advice-helpful-update elisp-demos-advice-describe-function-1
  :init
  ;; For traditional *Help*.
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  ;; For helpful.
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(provide 'init-tool)
;;; init-tool.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
