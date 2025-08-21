;;; init-misc-dsl.el -- Support miscellaneous DSL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dockerfile-ts-mode
  :init
  ;; Put the regex in the end, in case of mismatching a filename like "Dockerfile.org".
  (add-to-list 'auto-mode-alist
               '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode) t)
  )

;; Protobuf
(use-package protobuf-mode
  :init
  (celeste/prepare-package protobuf-mode)
  :mode ("\\.proto\\'" . protobuf-mode))


;; The following packages are managed in my Home-Manager configuration.

;; CMake (`cmake-mode.el' is provided by the cmake package)
(use-package cmake-mode
  :when (locate-library "cmake-mode")
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; From LLVM source, managed in my Home-Manager configuration.

(use-package tablegen-mode
  :when (locate-library "tablegen-mode")
  :mode ("\\.td\\'" . tablegen-mode)
  :config
  ;; `tablegen-mode' is not derived from `prog-mode', so I need to set some
  ;; hooks manually.
  (dolist (h '(font-lock-mode
               show-paren-local-mode
               display-line-numbers-mode
               hl-line-mode))
    (add-hook 'tablegen-mode-hook h))
  ;; Fix the broken pairing.
  (progn
    (modify-syntax-entry ?\(  "()"      tablegen-mode-syntax-table)
    (modify-syntax-entry ?\[  "(]"      tablegen-mode-syntax-table)
    (modify-syntax-entry ?\{  "(}"      tablegen-mode-syntax-table)
    (modify-syntax-entry ?\<  "(>"      tablegen-mode-syntax-table)
    (modify-syntax-entry ?\)  ")("      tablegen-mode-syntax-table)
    (modify-syntax-entry ?\]  ")["      tablegen-mode-syntax-table)
    (modify-syntax-entry ?\}  "){"      tablegen-mode-syntax-table)
    (modify-syntax-entry ?\>  ")<"      tablegen-mode-syntax-table)))

(use-package llvm-mode
  :when (locate-library "llvm-mode")
  :mode ("\\.ll\\'" . llvm-mode))

(use-package llvm-mir-mode
  :when (locate-library "llvm-mir-mode")
  :mode ("\\.mir\\'" . llvm-mir-mode))

(provide 'init-misc-dsl)
;;; init-misc-dsl.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
