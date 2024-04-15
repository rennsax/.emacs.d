;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Constants
(require 'init-const)

;;; Personal functions 🤠
(require 'init-func)

;;; Customization 🌷
(require 'init-custom)

;;; Package manager (in my case, w/ git submodules) and well-known deps 📦
(require 'init-package)

;;; Basic editor features 📝
(require 'init-editor)

;;; `evil-mode': the dark side of Emacs. 😈
;; Evil should be loaded very early, so other packages can define extra evil key
;; mappings.
;; Apograph from doom:
;; It is a story as old as time. A stubborn, shell-dwelling, and melodramatic
;; vimmer—envious of the features of modern text editors—spirals into despair
;; before he succumbs to the dark side.
(require 'init-evil)

;;;; Emacs awesome builtins. 🐂
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

;; TODO
(setq initial-buffer-choice celeste-init-file)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
