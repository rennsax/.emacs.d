;;; init-sh.el -- Shellscript support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :init
  (ide-zero-define sh
                   :mode sh-mode
                   :linter default)
  (sh-ide-zero-mode +1)
  :config
  (add-to-list 'auto-mode-alist `(,(rx ".zshrc" (* nonl) eos) . sh-mode))
  )

(provide 'init-sh)
;;; init-sh.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
