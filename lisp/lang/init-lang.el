;;; init-lang.el -- Language specified tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; IDE tools.

(require 'init-xref)
(require 'init-eglot)

(require 'init-flycheck)
(require 'init-snippet)
(require 'init-treesit)

(use-package ide-zero
  :init (celeste/prepare-package ide-zero)
  :commands ide-zero-define)



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



(provide 'init-lang)
;;; init-lang.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
