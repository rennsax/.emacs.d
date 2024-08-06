;;; init-cmake.el -- CMake support. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'init-cmake)
;;; init-cmake.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
