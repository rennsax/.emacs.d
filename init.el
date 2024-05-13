;;; init.el -- Entrypoint of init files. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Dependencies for initialization.
(require 'init-const)
(require 'init-lib)
;; init-custom is early-loaded in early-init.el
; (require 'init-custom)

;; Bootstrap `use-package'.
(require 'init-package)

;; Minimal but reasonable configurations.
(require 'init-mini)
(require 'init-mini-extra)

;; Fetch shell envs.
(require 'init-env)

;; Performance improvement.
(require 'init-perf)

;;; Personal functions 🤠
(require 'init-utils)

;;; Editor feature enhancement.
(require 'init-editor)

;;; Emacs awesome builtins. 🐂
(require 'init-eshell)
(require 'init-dired)

;;; Magical completion support 🧙🏻
(require 'init-treasure)

(require 'init-help)

(require 'init-corfu)

;;; Version control settings 🐱
(require 'init-vc)

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

(require 'init-private)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; no-byte-compile: t
;; End:
