;;; -*- lexical-binding: t -*-

(require 'init-const)

;;; Personal functions 🤠
(require 'init-func)

;;; Basic editor features 📝
(require 'init-editor)

;;; Emacs awesome builtins. 🐂
(require 'init-builtin)

;;; Magical completion support 🧙🏻
(require 'init-completion)

;;; Version control settings 🐱
(require 'init-vc)

;;; Miscellaneous tools 🔨
(require 'init-tool)

;;; `org-mode' support 🦄
(require 'init-org)

;;; Better CJK support 🀄
(require 'init-cjk)

;;; macOS defaults 🍎
(when sys/macp
  (require 'init-osx))

;;; UI tweaks 🔮
(require 'init-ui)

;;; Language extensions
(load (concat celeste-lisp-dir "lang/init"))


;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
