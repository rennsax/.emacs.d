;;; init-protobuf.el -- Protobuf support. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package protobuf-mode
  :init
  (celeste/prepare-package protobuf-mode)
  :mode ("\\.proto\\'" . protobuf-mode))

(provide 'init-protobuf)
;;; init-protobuf.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
