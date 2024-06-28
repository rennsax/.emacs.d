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

;; DSL.
(require 'init-cmake)
(require 'init-protobuf)
(require 'init-sh)
(require 'init-elisp)
(require 'init-nix)




(provide 'init-lang)
;;; init-lang.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
