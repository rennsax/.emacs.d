;;; init-misc-dsl.el -- Support miscellaneous DSL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Dockerfile
(celeste/package-autoload 'dockerfile-mode)

;; CMake (`cmake-mode.el' is provided by the cmake package)
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; Protobuf
(use-package protobuf-mode
  :init
  (celeste/prepare-package protobuf-mode)
  :mode ("\\.proto\\'" . protobuf-mode))


(provide 'init-misc-dsl)
;;; init-misc-dsl.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
