;;; init-protobuf.el -- Protobuf support. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(celeste/use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(provide 'init-protobuf)
;;; init-protobuf.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
