;;; init-sh.el -- Shellscript support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist `(,(rx ".zshrc" (* nonl) eos) . sh-mode))
  )

(provide 'init-sh)
;;; init-sh.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
