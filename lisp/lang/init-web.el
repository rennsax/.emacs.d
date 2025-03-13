;;; init-web.el -- Web development -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://medium.com/@fkmuiruri8/flycheck-eslint-and-emacs-993811736eb4

(use-package js
  :config
  (setq js-indent-level 2))

(use-package web-mode
  :init (celeste/prepare-package web-mode)
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.jsx?\\'" . web-mode))
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2)
  ;; This contiguously freezes my Emacs.
  (setq web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook (lambda () (setq-local standard-indent 2)))
  )

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; `web-mode' can be shared among css/html files, so only enable for files
  ;; with certain extensions.
  ;; Why `eval'? https://github.com/flycheck/flycheck/issues/1428#issuecomment-373896898
  (eval
   '(setf (flycheck-checker-get 'javascript-eslint 'predicate)
          (lambda () (when-let ((buf-name (buffer-file-name)))
                  (string-match-p (rx "." (| "ts" "tsx" "js" "jsx"))
                                  buf-name))))))

(celeste/setup-lang js
  :modes (web-mode)
  :project-identify "package.json")

(celeste/setup-lang tsx
  :modes (web-mode)
  :eglot-server ("typescript-language-server" "--stdio")
  :flycheck eglot)

;; Emmet support. Browse the cheatsheet: https://docs.emmet.io/cheat-sheet.
(use-package emmet-mode
  :commands emmet-mode
  :init (celeste/prepare-package emmet-mode)
  (add-hook 'web-mode-hook
            (defun +web-mode-enable-emmet-mode-h ()
              (emmet-mode +1)
              (pcase (buffer-file-name)
                ((rx "." (| "css" "scss" "less") eos)
                 (setq emmet-use-css-transform t))
                ((rx ".sass" eos)
                 (setq emmet-use-css-transform t)
                 (setq emmet-use-sass-syntax t)))))
  )

(provide 'init-web)
;;; init-web.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
