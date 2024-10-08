;;; init-misc-dsl.el -- Support miscellaneous DSL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dockerfile-ts-mode
  :init
  ;; Put the regex in the end, in case of mismatching a filename like "Dockerfile.org".
  (add-to-list 'auto-mode-alist
               '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode) t)
  )

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
