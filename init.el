;;; init.el -- Entrypoint of init files. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Dependencies for initialization.
(require 'init-const)
(require 'init-lib)

;;; Customization 🌷
(require 'init-custom)

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

(require 'init-project)

;;; Magical completion support 🧙🏻
(require 'init-treasure)

(require 'init-help)

(require 'init-window)

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

;;; Font settings 🔠
(when (display-graphic-p) (require 'init-font))

;;; UI tweaks 🔮
(require 'init-ui)

;;; Zen mode
(require 'init-zen)

;;; Powerful AI assistant.
(require 'init-ai)

;;; Language extensions
(add-to-list 'load-path (concat celeste-lisp-dir "lang"))
(require 'init-lang)

(require 'init-hydra)

(require 'init-temp nil t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; no-byte-compile: t
;; End:
