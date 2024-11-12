;;; init-sh.el -- Shellscript support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :init
  (add-to-list 'auto-mode-alist `(,(rx ".zshrc" (* nonl) eos) . sh-mode))
  (celeste/setup-lang sh
    :modes (sh-mode)
    :flycheck default
    :add-hook t)
  )

(provide 'init-sh)
;;; init-sh.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
