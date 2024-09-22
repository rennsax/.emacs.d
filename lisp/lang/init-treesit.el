;;; init-treesit.el -- Treesitter setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Treesitter: another Rust winner for syntax highlighting.

(use-package treesit
  :config
  ;; Level = 4: the highest level, which fontifies almost everything with TS.
  (setq treesit-font-lock-level 4))


(provide 'init-treesit)
;;; init-treesit.el ends here
