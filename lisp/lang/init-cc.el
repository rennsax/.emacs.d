;;; init-cc.el -- C/C++ support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package make
  ;; The default picked mode is `makefile-bsdmake-mode'.
  :mode (("[Mm]akefile\\'" . makefile-gmake-mode)))

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :config

  ;; NOTE: see `c-fallback-style' for fallback styles.
  (defconst celeste/cc-style
    '((c-basic-offset . 4)
      (c-comment-only-line-offset . 0)
      (c-hanging-braces-alist . (
                                 (brace-list-open)
                                 (inline-open after)
                                 (class-open after)
                                 (class-close)
                                 (defun-open after)
                                 ;;  (brace-entry-open)
                                 ;;  (statement-cont)
                                 ;;  (substatement-open after)
                                 ;;  (block-close . c-snug-do-while)
                                 ;;  (extern-lang-open after)
                                 (namespace-open after)
                                 ;;  (module-open after)
                                 ;;  (composition-open after)
                                 ;;  (inexpr-class-open after)
                                 ;;  (inexpr-class-close before)
                                 ;; (arglist-cont-nonempty)
                                 ))
      (c-offsets-alist . ((inline-open . 0)
                          (label . 0)
                          (innamespace . 0)
                          (inextern-lang . 0)))))
  (c-add-style "Celeste" celeste/cc-style)

  (celeste/add-mode-hook '(c-mode c++-mode)
      (defun celeste/cc-set-style ()
        (c-set-style "Celeste")
        (setq tab-width 4
              indent-tabs-mode nil)
        ;; (c-toggle-auto-newline +1)
        ))

  (celeste/setup-lang cc
    :modes (c-mode c-ts-mode c++-mode c++-ts-mode)
    :eglot-server ("ccls")
    :add-hook t
    :flycheck eglot)
  )

(provide 'init-cc)
;;; init-cc.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
