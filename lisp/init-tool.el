;;; init-tool.el -- Tools integration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Yet another great "ripgrep" frontend!
(use-package deadgrep
  :init
  (celeste/prepare-package s)
  (celeste/prepare-package dash)
  (celeste/prepare-package spinner)
  (celeste/prepare-package deadgrep)

  :bind ("C-c r g" . deadgrep)
  :config
  ;; Use the standard `display-buffer' so we can customize the behavior in `display-buffer-alist'.
  (setq deadgrep-display-buffer-function #'display-buffer)

  (define-advice deadgrep-restart (:around (oldfun &rest args) keep-cursor)
    "Try to persist the cursor position after restarting deadgrep."
    (let ((cur (point)))
      (apply oldfun args)
      (goto-char cur)))

  ;; -z, --search-zip: also search in compressed files.
  (add-to-list 'deadgrep-extra-arguments "-z")

  (defun deadgrep-specify-extra-arguments ()
    (interactive)
    (let ((arg-str (read-from-minibuffer
                 "Extra arugments: "
                 (concat (string-join deadgrep-extra-arguments " ") " "))))
      (setq-local deadgrep-extra-arguments
                  (string-split arg-str))))
  (keymap-set deadgrep-mode-map "A" #'deadgrep-specify-extra-arguments)
  )

;; Edit anything, everywhere, w/ an popped Emacs frame!
(use-package emacs-everywhere
  :init
  (celeste/prepare-package emacs-everywhere)
  :commands emacs-everywhere)

;; Enchanted spell checker.
(use-package jinx
  :init
  (celeste/prepare-package compat)
  (celeste/prepare-package jinx)

  :diminish
  :commands jinx-mode
  :config
  (setq jinx-languages "en_US")
  ;; Exclude Chinese characters. This should be a universal setting, so I put it
  ;; here (instead of init-cjk.el)
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))

  (with-eval-after-load 'vertico-multiform
    ;; Use grid view for `jinx-correct' completion menu. Recommended by minad himself.
    (require 'vertico-grid)
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))
    )
  (keymap-global-set "M-$" #'jinx-correct)

  :init
  (defcustom jinx-mode-dict-alist
    '((emacs-lisp-mode ("el"))
      (python-mode ("numpy"))
      (go-mode ("fmt")))
    "Mode-local dictionaries."
    :group 'jinx
    ;; TODO type?
    :type '(list (symbol (list string))))

  (defcustom jinx-enable-mode-list
    '(prog-mode text-mode)
    "Modes that jinx should be enabled."
    :group 'jinx
    :type '(list (symbol)))

  ;; Manually setup mode-local words and enable jinx in specified modes.
  (mapc (lambda (mode-word-list)
          (let ((mode (car mode-word-list))
                (word-list (cadr mode-word-list)))
            (add-hook (intern (concat (symbol-name mode) "-hook"))
                      #'(lambda ()
                          (setq jinx-local-words
                                (mapconcat #'identity word-list " "))
                          (jinx-mode +1)))))
        jinx-mode-dict-alist)
  ;; Must *after* setting `jinx-local-words'.
  (celeste/add-mode-hook jinx-enable-mode-list #'jinx-mode)
  )


;;; Preview math formulas inline with MathJax.

(use-package math-preview
  :init
  (celeste/prepare-package (s dash math-preview))
  (with-eval-after-load 'org
    (defalias #'org-latex-preview #'math-preview-at-point)
    (defalias #'org-clear-latex-preview #'math-preview-clear-region))
  :commands math-preview-at-point math-preview-clear-region)


;;; Undo system enhancement.

;; Visualized undo history.
(celeste/prepare-package vundo)
(use-package vundo
  :commands vundo)

;; Persistent undo history across Emacs sessions.
(celeste/prepare-package undo-fu-session)
(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :init
  (celeste/prepare-package undo-fu-session)

  (setq undo-fu-session-directory (celeste/make-path "undo-fu-session" 'state))

  :config
  (setq undo-fu-session-incompatible-files
        '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))


;;; Elfeed: read elfeed in Emacs!

;; NOTE: To make `elfeed-show-mode' display images normally, remember to compile
;; Emacs with options "--with-imagemagick"
(use-package elfeed
  :init (celeste/prepare-package elfeed)
  :commands elfeed
  :config
  (defun +elfeed-improve-readability ()
    (text-scale-set +2)
    (visual-line-mode +1))

  (add-hook 'elfeed-search-mode-hook #'+elfeed-improve-readability)
  (add-hook 'elfeed-show-mode-hook #'+elfeed-improve-readability)

  (define-advice elfeed-kill-buffer (:after (&rest _) close-tab)
    (tab-bar-close-tab)))



(provide 'init-tool)
;;; init-tool.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
