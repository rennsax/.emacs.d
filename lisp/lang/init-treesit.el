;;; init-treesit.el -- Treesitter setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Treesitter: another Rust winner for syntax highlighting.

(use-package treesit
  :config
  ;; Level = 4: the highest level, which fontifies almost everything with TS.
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :init
  (celeste/prepare-package treesit-auto)
  :config
  ;; NOTE: Cpp parser is based on C parser. Remember to install them simultaneously.

  ;; Initialize `treesit-language-source-alist'. By default it's nil in Emacs.
  (setq treesit-language-source-alist
        (treesit-auto--build-treesit-source-alist))
  ;; I install grammar manually.
  (setq treesit-auto-install nil)

  (defun +treesit-auto-install (lang &optional ensure)
    "Install LANG grammar. LANG is a symbol of the language to be installed.

If ENSURE is non-nil, do nothing if the grammar for LANG has been installed."
    (interactive (list (intern
                        (completing-read
                         "Language: "
                         (mapcar #'car treesit-language-source-alist)))))
    (let* ((recipe (seq-filter (lambda (recipe) (eq (car recipe) lang)) treesit-language-source-alist))
           (installed (treesit-ready-p lang t))
           (skip (and installed ensure)))
      (if skip t
        (if recipe
            (when (and (or (not installed)
                           (y-or-n-p (format "Grammar for %s has been installed. Reinstall it?" lang)))
                       (y-or-n-p (format "Install grammar for %s?" lang)))
              (treesit-install-language-grammar lang))
          (progn (message "No recipe for %s. Make sure you did not modify `treesit-auto-langs'." lang)
                 nil)))))
  )



(provide 'init-treesit)
;;; init-treesit.el ends here
