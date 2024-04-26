;;; init-protobuf.el -- Protobuf support. -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(celeste/use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  :commands protobuf-mode)

(provide 'init-protobuf)
;;; init-protobuf.el ends here
