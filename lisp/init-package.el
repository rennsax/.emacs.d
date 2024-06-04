;;; init-package.el -- Package initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; use-package: convenient macro to customize Emacs packages.

(require 'use-package)

;; Enable `use-package'.
(setq use-package-always-ensure nil ; do not install packages w/ package.el!
      use-package-always-defer t ; to accelerate startup
      use-package-expand-minimally t)

;; I feel hard to remember whether to use the plural...
(defalias 'use-package-handler/:command 'use-package-handler/:commands)
(add-to-list 'use-package-keywords :command)
(defalias 'use-package-handler/:function 'use-package-handler/:functions)
(add-to-list 'use-package-keywords :function)


;;; diminish - change or conceal the mode-line indicator for minor modes.

(celeste/require 'diminish)


;;; GNU elpa

;; Override Emacs's builtin packages here. It should be early enough, in case of
;; required by other packages.

(celeste/prepare-package jsonrpc)
(celeste/prepare-package eldoc)


(provide 'init-package)
;;; init-package.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
