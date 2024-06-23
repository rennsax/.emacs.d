;;; init-ui.el -- UI tweaks. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



;;; Scrolling behavior.

(setq hscroll-step 1
      hscroll-margin 2
      scroll-margin 3 ; Like "scrolloff" in VIM
      ;; If scroll more than 20 lines (e.g. 30j) out of the screen, recenter the screen.
      ;; By default, the value is 0, which means Emacs always recenters the screen when
      ;; the cursor moves out.
      scroll-conservatively 20
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; New feature in Emacs 29.1! Smooth scrolling!
(pixel-scroll-precision-mode +1)


;;; General UI.

;; Disable the cute blinking cursor.
(blink-cursor-mode -1)

;; Highlight the current line. This is achived by putting an overlay.
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Show matched parentheses.
(use-package paren
  :init
  (add-hook 'prog-mode-hook #'show-paren-local-mode)
  (setq show-paren-when-point-inside-paren t)
  :config
  ;; By default it's enabled after Emacs is launched.
  (show-paren-mode -1)
  )

(use-package elisp-mode
  :config
  ;; Show "lambda" keyword as λ.
  (add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  ;; Why? Because it's inconvenient to move "logically". Why not just use
  ;; `goto-line' or `avy-goto-line'?
  (setq display-line-numbers-type t)
  ;; Suitable for line number over thousands.
  (setq-default display-line-numbers-width 4))


;;; Window behavior.

;; Do not allow splitting a window vertically.
(setq split-height-threshold nil)


;;; Colors, font faces.

(use-package hl-todo
  :init
  (celeste/prepare-package compat)
  (celeste/prepare-package hl-todo)
  :hook ((prog-mode yaml-mode) . hl-todo-mode)
  :defines (hl-todo-highlight-punctuation
            hl-todo-keyword-faces)
  :config
  ;; Copied from doom
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("PERF" font-lock-keyword-face bold)
          ("XXX" font-lock-constant-face bold))))

;; Show colors in the buffer as the background, for example, white red.
(use-package rainbow-mode
  :init
  (celeste/prepare-package rainbow-mode)
  :commands rainbow-mode
  :config
  ;; If `rainbow-mode' is on, disable `hl-line-mode', which can override the
  ;; background color.
  (add-hook 'rainbow-mode-hook
            (defun +rainbow-mode-repel-hl-line-mode-h ()
              (hl-line-mode (if rainbow-mode -1 +1)))))

;; Rainbow delimiters.
(use-package rainbow-delimiters
  :init
  (celeste/prepare-package rainbow-delimiters)
  ;; Start automatically in most programming languages.
  :hook (prog-mode . rainbow-delimiters-mode))

;; Oh, the vanilla dired is boring! I want more colors.
(use-package diredfl
  :init
  (celeste/prepare-package diredfl)
  :hook (dired-mode . diredfl-mode))



;;; Theme

(use-package doom-themes
  :init
  (celeste/prepare-package doom-themes)
  :demand t
  :preface
  (defun +load-default-theme ()
    "Load the default theme."
    (interactive)
    (load-theme celeste-default-theme 'no-comfirm))

  (defun +custom-doom-themes (&rest args)
    "Wrapper for `custom-set-faces'. ARGS are transfered before passed."
    (apply #'custom-set-faces
           (mapcar (lambda (arg)
                     `(,(car arg) ((t . (,@(cdr arg)))))) args)))

  (defun +doom-themes-custom ()
    "Extra customization for doom themes.

This should be called each time after the function definition is modified."
    (interactive)
    (let ((doc-font celeste-other-font-name)
          (code-font celeste-default-font-name)
          (red (doom-color 'red))
          (yellow (doom-color 'yellow))
          (orange (doom-color 'orange))
          (magenta (doom-color 'magenta))
          (cyan (doom-color 'cyan))
          (dark-cyan (doom-color 'dark-cyan))
          (bg (doom-color 'bg)))
      (+custom-doom-themes

       ;; `magit-mode'
       `(magit-branch-remote :box (:line-width (-1 . -1)) :weight bold)
       `(magit-section-heading :foreground ,(doom-color 'yellow))
       `(magit-branch-current :box (:line-width (-1 . -1)) :weight bold
                              :foreground ,(doom-color 'dark-red))

       ;; avy
       `(avy-background-face :foreground ,dark-cyan)
       `(avy-lead-face :background ,red)

       ;; `(olivetti-fringe :background ,(doom-darken bg 0.1))

       ;; vertico-posframe
       `(vertico-posframe-border :background ,yellow)

       ;; tab-bar
       `(tab-bar :background ,(doom-color 'base3) :height 1.1)
       `(tab-bar-tab-inactive :background ,(doom-color 'base3))

       ;; `org-mode'
       `(org-block :family ,code-font)
       `(org-code :family ,code-font)
       `(org-verbatim :family ,code-font)
       `(org-quote :family ,doc-font :italic nil :foreground ,cyan)
       `(org-bold :weight bold :foreground ,yellow)
       `(org-indent :background ,(doom-darken (doom-color 'base4) 0.1)
                                  :foreground ,(doom-darken (doom-color 'base4) 0.1))

       `(org-table :family ,celeste-really-mono-font-name)

       `(aw-mode-line-face :foreground ,(doom-darken 'orange 0.1))
       `(aw-leading-char-face :foreground ,yellow :background ,red :height 2.0)

       ;; `markdown-mode'
       `(markdown-inline-code-face :family ,code-font :foreground ,orange)
       `(markdown-code-face :family ,code-font)
       `(markdown-blockquote-face :foreground ,cyan)
       `(markdown-header-delimiter-face :foreground ,(doom-color 'dark-cyan))
       `(markdown-header-face-1 :inherit outline-1 :underline t)
       `(markdown-header-face-2 :inherit outline-2 :underline t)
       `(markdown-header-face-3 :inherit outline-3 :underline t)
       `(markdown-header-face-4 :inherit outline-4)
       `(markdown-header-face-5 :inherit outline-5)

       ;; `show-paren-mode' TODO: if configured here, the box color is strange.
       ;; use `(-1 . -1)' to avoid any increase in the character height or width
       ;; `(show-paren-match :box (:line-width (-1 . -1)) :weight ultra-bold)

       ;; `evil-goggles'
       `(evil-goggles-default-face :background ,(doom-color 'yellow))

       ;; `diredfl-mode'
       `(diredfl-dir-name :foreground ,(doom-color 'cyan) :weight bold)
       `(diredfl-symlink :foreground ,(doom-color 'red)))))
  :hook (after-init . (lambda () (+load-default-theme) (+doom-themes-custom))))


;;; Modeline

;; Information that should be displayed in modeline:
;; the size of the buffer
(size-indication-mode +1)
;; colum number
(column-number-mode +1)
;; NO line number
(line-number-mode -1)

(celeste/add-mode-hook '(eshell-mode shell-mode vterm-mode) #'hidden-mode-line-mode)


;;; Completion.

(use-package nerd-icons
  :init
  (celeste/prepare-package nerd-icons))

(use-package nerd-icons-completion
  :init
  (celeste/prepare-package nerd-icons-completion)
  :after marginalia
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup))
  :config
  (nerd-icons-completion-mode +1))

(use-package nerd-icons-corfu
  :init (celeste/prepare-package nerd-icons-corfu)
  :after corfu
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;;; pulsar

;; Never feel lost after executing some navigation commands!
(use-package pulsar
  :init
  (celeste/prepare-package pulsar)
  ;; Evil has its own solution: `evil-goggles'.
  :when (not (eq celeste-modal-editing 'evil))
  :hook (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t
        pulsar-iterations 10)

  (setq pulsar-pulse-functions
        (append pulsar-pulse-functions
                `(scroll-up-half scroll-down-half other-window-or-switch-buffer))))



(provide 'init-ui)
;;; init-ui.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
