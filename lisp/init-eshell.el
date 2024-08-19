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
  :bind (("C-c b e" . eshell))
  :config

  (setq eshell-scroll-to-bottom-on-input t)
  ;; More history!
  (setq eshell-history-size 10000
        ;; Duplicates are bad, bad, bad!
        eshell-hist-ignoredups t)

  (load (concat celeste-autoload-dir "eshell-prompt"))
  (setq eshell-prompt-function '+eshell-default-prompt-fn)

  ;; Eshell prompt depends on `shrink-path'.
  (celeste/prepare-package s)
  (celeste/prepare-package f)
  (celeste/prepare-package dash)
  (celeste/prepare-package shrink-path)
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
(use-package eshell-syntax-highlighting
  :after esh-mode
  :init (celeste/prepare-package eshell-syntax-highlighting)
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package em-rebind
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :config
  (add-hook 'eshell-mode-hook (lambda () (electric-pair-local-mode -1)))
  (defun eshell-yank-last-arg ()
    "Insert the last arg of the previous command."
    (interactive)
    (insert "$_")
    (pcomplete-expand))
  (bind-keys :map eshell-mode-map
             ("C-d" . eshell-delchar-or-maybe-eof)
             ("M-." . eshell-yank-last-arg)))


(provide 'init-eshell)
;;; init-eshell.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
