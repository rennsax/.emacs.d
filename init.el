;;; init.el -- Entrypoint of init files. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Dependencies for initialization.
(require 'init-const)
(require 'init-lib)

;; Bootstrap `use-package'.
(require 'init-package)

;; Minimal but reasonable configurations.
(require 'init-mini)
(require 'init-mini-extra)

;; Fetch shell envs.
(require 'init-env)

;;; Personal functions 🤠
(require 'init-func)

;;; Editor feature enhancement.
(require 'init-editor)

;;; Emacs awesome builtins. 🐂
(require 'init-eshell)
(require 'init-dired)

;;; Magical completion support 🧙🏻
(require 'init-completion)

(require 'init-help)

(require 'init-corfu)

;;; Version control settings 🐱
(require 'init-vc)

;;; Project management.
(require 'init-project)

;;; Virtual terminal.
(require 'init-vterm)

;;; Miscellaneous tools 🔨
(require 'init-tool)

;;; `org-mode' support 🦄
(require 'init-org)

;;; Better CJK support 🀄
(require 'init-cjk)

;;; macOS defaults 🍎
(when sys/mac
  (require 'init-osx))

;;; UI tweaks 🔮
(require 'init-ui)

;;; Language extensions
(load (concat celeste-lisp-dir "lang/init"))

;;; Powerful AI assistant.
(require 'init-ai)

(require 'init-temp)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; no-byte-compile: t
;; End:
