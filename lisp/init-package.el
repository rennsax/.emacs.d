;;; init-package.el -- Package initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

;; Enable `use-package'.
(setq use-package-always-ensure nil ; do not install packages w/ package.el!
      use-package-always-defer t ; to accelerate startup
      use-package-expand-minimally t)

;; diminish - change or conceal the mode-line indicator for minor modes.
(celeste/require 'diminish)


(provide 'init-package)
;;; init-package.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
