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
      mouse-wheel-scroll-amount-horizontal 2
      )

;; New feature in Emacs 29.1! Smooth scrolling!
(pixel-scroll-precision-mode +1)


;;; General UI.

;; Disable the cute blinking cursor.
(blink-cursor-mode -1)

;; Highlight the current line. This is achived by putting an overlay.
(global-hl-line-mode)

;; Show matched parentheses.
(show-paren-mode)

(celeste/add-mode-hook '(prog-mode text-mode) #'display-line-numbers-mode)
;; Display relative line numbers.
(setq display-line-numbers-type 'relative)


;;; Window behavior.

;; Do not allow splitting a window vertically.
(setq split-height-threshold nil)


;;; Colors, font faces.

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

;; Show colors in the buffer as the background, for example, white red.
(celeste/use-package rainbow-mode
  :commands rainbow-mode
  :config
  ;; If `rainbow-mode' is on, disable `hl-line-mode', which can override the
  ;; background color.
  (add-hook 'rainbow-mode-hook
            (defun +rainbow-mode-repel-hl-line-mode-h ()
              (hl-line-mode (if rainbow-mode -1 +1)))))

;; Rainbow delimiters.
(celeste/use-package rainbow-delimiters
  ;; Start automatically in most programming languages.
  :hook (prog-mode . rainbow-delimiters-mode))

;; Oh, the vanilla dired is boring! I want more colors.
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



;;; Popper.

;; `popper': show annoying windows such as `help-mode' in a dedicated POP
;; window, so they won't clobber the original window layout.
(add-to-list 'load-path (concat celeste-package-dir "popper"))
(use-package popper
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
          "\\*Flycheck checkers\\*"
          ;; Exclude "*Org Help*" buffer in `org-goto'. If not, `org-goto'
          ;; firstly focuses on the popper window, which is annoying.
          (lambda (buf)
            (with-current-buffer buf
              (and (derived-mode-p 'help-mode)
                   (not (string= (buffer-name) "*Org Help*")))))
          helpful-mode ; `helpful' package
          debugger-mode)))

(use-package popper-echo
  :after popper
  :commands popper-echo-mode popper-tab-line-mode
  :hook ((popper-mode . (lambda ()
                          (popper-tab-line-mode -1)
                          (popper-echo-mode +1)))))


;;; Theme

(celeste/use-package doom-themes
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
          (yellow (doom-color 'yellow))
          (orange (doom-color 'orange))
          (magenta (doom-color 'magenta))
          (cyan (doom-color 'cyan)))
      (+custom-doom-themes

       ;; TODO: dream to use another font in vertico

       ;; `magit-mode'
       `(magit-branch-remote :box (:line-width (-1 . -1)) :weight bold)
       `(magit-section-heading :foreground ,(doom-color 'yellow))
       `(magit-branch-current :box (:line-width (-1 . -1)) :weight bold
                              :foreground ,(doom-color 'dark-red))

       ;; `org-mode'
       `(org-block :family ,code-font)
       `(org-code :family ,code-font)
       `(org-verbatim :family ,code-font)
       `(org-quote :family ,doc-font :italic nil :foreground ,cyan)
       `(org-bold :weight bold :foreground ,yellow)

       ;; `markdown-mode'
       `(markdown-inline-code-face :family ,code-font)
       `(markdown-code-face :family ,code-font)
       `(markdown-blockquote-face :foreground ,cyan)
       `(markdown-header-delimiter-face :foreground ,(doom-color 'dark-cyan))
       `(markdown-header-face-1 :inherit outline-1)
       `(markdown-header-face-2 :inherit outline-2)
       `(markdown-header-face-3 :inherit outline-3)
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
  :hook (after-init . +load-default-theme))


;;; Modeline

;; Information that should be displayed in modeline:
;; the size of the buffer
(size-indication-mode +1)
;; colum number
(column-number-mode +1)
;; NO line number
(line-number-mode -1)

(celeste/add-mode-hook '(eshell-mode shell-mode vterm-mode) #'hidden-mode-line-mode)



(provide 'init-ui)
;;; init-ui.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
