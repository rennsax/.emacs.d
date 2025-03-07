;;; init-help.el -- Make Emacs manual more readable -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;;; GNU Info
(use-package info
  :init
  (defun info-refresh ()
    "Refresh current `Info-directory-list'.

This function is useful after you change `Info-default-directory-list'."
    (interactive)
    (setq Info-directory-list nil)
    (info-initialize))

  (defun info-show (string)
    "Show info file found by `info' program with STRING as the argument."
    (interactive "sInfo manual: \n")
    (if-let* ((str (shell-command-to-string (format "info -w %s" string)))
                (index (string-match (rx (group (* nonl)) "\n") str))
                (info-file-path (match-string 1 str)))
        (info info-file-path)
      (error "Cannot open info page for %s!" string)))
  )




;;; man integration

(use-package man
  :init
  ;; Generalized interface to browse Linux manual page.
  (defvar manual-program)
  (defvar linux-manual-dir "~/.local/share/linux-man")
  (defun +darwin-lman (&optional man-arg)
    "Browse Linux manual on macOS."
    (interactive)
    (let ((manual-program (concat "man -M " linux-manual-dir)))
      (if man-arg
          (man man-arg)
        (call-interactively #'man))))
  (cond (sys/mac (defalias 'lman '+darwin-lman))
        (sys/linux (defalias 'lman 'man)))

  ;; Bind keys.
  (bind-keys ("C-c m m" . man)
             ("C-c m l" . lman))

  (when (and sys/mac (executable-find "gman"))
    ;; Check: https://github.com/abo-abo/swiper/issues/2836#issuecomment-831292443.
    ;; 1. Install man-db (nongnu's implementation).
    ;; 2. Run `mandb' in the shell to build cache.
    (setq manual-program "gman"))

  ;; Integration with embark
  (with-eval-after-load 'embark
    (keymap-set embark-identifier-map "M m" #'man)
    (keymap-set embark-identifier-map "M l" #'lman))
  )



;; Better Emacs *help* buffer that provides much more contextual information.
;; For example, source code, references, key bindings, ...
(use-package helpful
  :init
  ;; dependency
  (celeste/prepare-package (s dash elisp-refs helpful))

  (defun +helpful-set-as-original (sym)
    "Restore the value of SYM to its original value, if any."
    (interactive
     (list (helpful--read-symbol
            "Variable: "
            (helpful--variable-at-point)
            #'helpful--variable-p)))
    (if-let ((ori-val (car-safe (helpful--original-value sym))))
        (set sym ori-val)
      (user-error "The original value of %s cannot be found!" sym)))

  :bind (([remap describe-function] . helpful-callable) ; `helpful-function' excludes macros
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("C-h h" . helpful-at-point)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-command] . helpful-command))
  :config
  (add-hook 'helpful-mode-hook #'visual-line-mode))

;; Show demos in the *Help* or `helpful-mode' buffer.
(use-package elisp-demos
  :init
  (celeste/prepare-package elisp-demos)
  :commands elisp-demos-advice-helpful-update elisp-demos-advice-describe-function-1
  :init
  ;; For traditional *Help*.
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  ;; For helpful.
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))


(provide 'init-help)
;;; init-help.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
