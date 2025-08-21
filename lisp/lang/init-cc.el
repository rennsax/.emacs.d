;;; init-cc.el -- C/C++ support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package make
  ;; The default picked mode is `makefile-bsdmake-mode'.
  :mode (("[Mm]akefile\\'" . makefile-gmake-mode)))

(use-package cc-mode
  :init
  :config
  (setq c-default-style "k&r"
        c-basic-offset 4)

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
