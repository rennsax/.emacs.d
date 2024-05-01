;;; init-eshell.el -- Eshell enhancement. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Emacs shell - wow, such a versatile and powerful shell, seamlessly
;;; integrated with Emacs itself! 🐚
(use-package eshell
  :defines (eshell-scroll-to-bottom-on-input
            eshell-history-size
            eshell-prompt-function
            eshell-mode-map)

  :config

  (setq eshell-scroll-to-bottom-on-input t)
  ;; More history!
  (setq eshell-history-size 10000)

  (load (concat celeste-autoload-dir "eshell-prompt"))
  (setq eshell-prompt-function '+eshell-default-prompt-fn)

  (celeste/autoload '+eshell-input-bol eshell)
  (celeste/autoload '+eshell-kill-whole-input eshell))

;; To most programs, eshell is dumb terminal. Therefore we need to tell Eshell
;; to open up visual commands in a dedicated terminal emulator.
(use-package em-term
  :config
  (setq eshell-visual-subcommands
        '(("git" "log" "ls" "diff" "difftool" "show")))
  (add-to-list 'eshell-visual-commands "nvim"))

;; Syntax highlighting.
(with-eval-after-load 'esh-mode
  (celeste/require 'eshell-syntax-highlighting)
  (eshell-syntax-highlighting-global-mode +1))


(provide 'init-eshell)
;;; init-eshell.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
