;;; init-lang.el -- Language specified tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; IDE tools.

(require 'init-xref)
(require 'init-eglot)

(require 'init-flycheck)
(require 'init-format)
(require 'init-snippet)
(require 'init-treesit)
(require 'init-docs)
(require 'init-ctags)

(eval-when-compile
  (celeste/prepare-package ide-zero)
  (require 'ide-zero))



;;; Programming Languages.

;; Markup.
(require 'init-yaml)
(require 'init-markdown)
(require 'init-json)

;; General-purpose.
(require 'init-go)
(require 'init-lua)
(require 'init-cc)
(require 'init-python)
(require 'init-rust)

;; DSL.
(require 'init-sh)
(require 'init-elisp)
(require 'init-nix)

;; Misc.
(require 'init-misc-dsl)
(require 'init-beancount)



(provide 'init-lang)
;;; init-lang.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
