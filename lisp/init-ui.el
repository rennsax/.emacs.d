;;; init-ui.el -- UI tweaks. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-package))

(require 'init-custom)

;; Prefer simpler "y" or "n" over "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)

;; Scrolling behavior.
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
      mouse-wheel-scroll-amount-horizontal 2
      )

;; New feature in Emacs 29.1! Smooth scrolling!
(when emacs/>=29p
  (pixel-scroll-precision-mode +1))

;; Disable the cute blinking cursor.
(blink-cursor-mode -1)

;; Highlight the current line. This is achived by putting an overlay.
(global-hl-line-mode)

;; Show matched parentheses.
(show-paren-mode)

;; Toggle `display-fill-column-indicator-mode' along with `auto-fill-mode'.
;; Show vertical line at the column of `fill-column'.
;; TODO: how to tell whether `auto-fill-mode' is enabled?
(add-hook 'auto-fill-mode-hook
          (lambda () (display-fill-column-indicator-mode 'toggle)))

;; Set different fonts for those special modes, so I can be awared of different contexts.
(celeste/add-mode-hook celeste-other-font-mode-list
    (defun +buffer-set-other-font ()
      "Setup another font for the current buffer."
      (setq-local buffer-face-mode-face (list :family celeste-other-font-name))
      (buffer-face-mode)))

(use-package display-line-numbers
  ;; Display relative line numbers.
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;; `popper': show annoying windows such as `help-mode' in a dedicated POP
;; window, so they won't clobber the original window layout.
(celeste/use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :hook (after-init . popper-mode)
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Org Agenda\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*evil-jumps\\*"
          "\\*Compile-Log\\*"
          "\\*compilation\\*"
          help-mode
          helpful-mode ; `helpful' package
          debugger-mode))
  (use-package popper-echo
    :commands popper-echo-mode
    :hook (popper-mode . popper-echo-mode)))

(celeste/use-package hl-todo
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

;; used by `doom-modeline'
(celeste/use-package nerd-icons)

(celeste/use-package doom-themes
  :demand t
  :preface
  (defun +load-default-theme ()
    "Load the default theme."
    (interactive)
    (load-theme celeste-default-theme 'no-comfirm))

  :hook (after-init . (lambda () (+load-default-theme) (+doom-themes-custom)))
  :config
  (defun +custom-doom-themes (&rest args)
    "Wrapper for `custom-set-faces'. ARGS are transfered before passed."
    (apply #'custom-set-faces
           (mapcar (lambda (arg)
                     `(,(car arg) ((t . (,@(cdr arg)))))) args)))

  (defun +doom-themes-custom ()
    "Extra customization for doom themes."
    (+custom-doom-themes
     `(magit-branch-remote :box (:line-width (-1 . -1)) :weight bold)
     `(magit-section-heading :foreground ,(doom-color 'yellow))
     `(magit-branch-current :box (:line-width (-1 . -1)) :weight bold
                            :foreground ,(doom-color 'dark-red))
     ))
  )

(celeste/use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(celeste/use-package visual-fill-column)
(celeste/use-package writeroom-mode
  :commands writeroom-mode
  :config
  ;; Preserve the mode line.
  (setq writeroom-mode-line t)
  (setq writeroom-global-effects (delq 'writeroom-set-fullscreen writeroom-global-effects)))


;; Better Emacs *help* buffer that provides much more contextual information.
;; For example, source code, references, key bindings, ...
(celeste/use-package elisp-refs) ; dep
(celeste/use-package helpful
  :hook (helpful-mode . visual-line-mode) ; turn on word wrap
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;; Show colors in the buffer as the background, for example, white red.
(celeste/use-package rainbow-mode
  :commands rainbow-mode
  :config
  ;; If `rainbow-mode' is on, disable `hl-line-mode', which can override the
  ;; background color.
  (add-hook 'rainbow-mode-hook
            (lambda () (hl-line-mode (if rainbow-mode -1 +1)))))

;; Rainbow delimiters.
(celeste/use-package rainbow-delimiters
  ;; Start automatically in most programming languages.
  :hook (prog-mode . rainbow-delimiters-mode))

;; Oh, the venilla dired is boring! I want more colors.
(celeste/use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Icons are also important!
(celeste/use-package nerd-icons-dired
  :diminish
  :custom-face
  ;; TODO doc
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :commands (nerd-icons-dired-mode)
  ;; :hook (dired-mode . nerd-icons-dired-mode)
  )


(provide 'init-ui)
;;; init-ui.el ends here
