;;; init-format.el -- Code formatter -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Code formatter.
(use-package format-all
  :init
  (celeste/prepare-package (language-id inheritenv format-all))
  :commands format-all-region-or-buffer format-all-buffer
  ;; REVIEW: maybe add `eglot--signal-textDocument/didChange' to
  ;; `format-all-after-format-functions'?
  )



(provide 'init-format)
;;; init-format.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
