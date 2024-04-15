;;; init-package.el -- Package initialization. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const))

;; Enable `use-package'.
(celeste/require use-package)
(setq use-package-always-ensure nil ; do not install packages w/ package.el!
      use-package-always-defer t ; to accelerate startup
      use-package-expand-minimally t)

;; Common Lisp extensions for Emacs, builtin
(celeste/require dash)
(celeste/require s)
(celeste/require f)
(celeste/require annalist)
(celeste/require compat) ; COMPATibility Library for Emacs Lisp
(celeste/require shrink-path)

;; REVIEW: `doom-modeline' does not show any minor mode factually. So do I still
;; need diminish.el? IDK, so I specify it as a lazy-loaded dependency.
(celeste/use-package diminish
  :commands (diminish diminish-undo diminished-mode))

(provide 'init-package)
;;; init-package.el ends here
