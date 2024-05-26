;;; lang/init.el -- Entrypoint for language specified configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; When the file is loaded, add its parent directory to `load-path'.
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))


;;; Syntax checker.

;; flycheck: successor of the builtin flymake.
(use-package flycheck
  :init
  (celeste/prepare-package flycheck)
  :config
  ;; Check syntax when: the file is saved, a short time
  ;; (`flycheck-idle-change-delay') after the last change, and immediately after
  ;; `flycheck-mode' is enabled.
  ;; Of course, use `flycheck-buffer' to manually check the syntax.
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  ;; My load-path
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Default is 400. If more than threshold, an annoying warning appears.
  (setq flycheck-checker-error-threshold 1000)

  )

;; `consult-flycheck'
(use-package consult-flycheck
  :after flycheck
  :init
  (celeste/prepare-package consult-flycheck)
  :bind (("C-c s d" . consult-flycheck)))

;; Show diagnostic information in the buffer.
(use-package sideline
  :init
  (celeste/prepare-package ht)
  (celeste/prepare-package sideline)
  :diminish
  :config
  (setq sideline-backends-left-skip-current-line t
        ;; Allow right sideline at the current line
        sideline-backends-right-skip-current-line nil
        sideline-order-right 'down
        sideline-order-left 'down
        sideline-priority 100 ; overlays' priority
        sideline-delay 1.0 ; longer delay
        )
  ;; More eye-catching annotation.
  (setq sideline-format-left "%s   "
        sideline-format-right "   %s"
        sideline-display-backend-name t
        sideline-display-backend-format "[%s]")

  ;; TODO sideline is a pretty package, but it's too young, and I've discovered
  ;; some bugs:
  ;; 1. `sideline-delay' is not respected
  ;; 2. when sideline is show, then `revert-buffer', the sideline will freeze
  ;; there.
  )

(use-package sideline-flycheck
  :init
  (celeste/prepare-package sideline-flycheck)
  :hook ((flycheck-mode . sideline-mode)
         (flycheck-mode . sideline-flycheck-setup))
  :config
  ;; (setq sideline-backends-right '(sideline-flycheck))
  (setq sideline-backends-left '(sideline-flycheck))
  )


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


;;; LSP

;; Snippet engine, integrated with Elgot, lsp-mode and lsp-bridge nicely.
(use-package yasnippet
  :init
  (celeste/prepare-package yasnippet)
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1)
  (bind-keys :map yas-keymap
             ("RET" . yas-next-field)
             ("M-RET" . yas-prev-field))
  (unbind-key "TAB" yas-keymap))

(use-package lsp-bridge
  :diminish lsp-bridge-mode
  :commands lsp-bridge-mode global-lsp-bridge-mode
  :config
  ;; Enable tempel.
  (with-eval-after-load 'tempel
    (setq acm-enable-tempel t))
  ;; RET should just insert a newline, completion is done by TAB.
  (mapc (lambda (key) (keymap-unset acm-mode-map key))
        '("RET" "<remap> <next-line>" "<remap> <previous-line>"
          "<remap> <beginning-of-buffer>"))

  ;; corfu-mode is conflicted with lsp-bridge-mode.
  (with-eval-after-load 'corfu
    (add-hook 'lsp-bridge-mode-hook
              (defun +lsp-bridge-mode-repel-corfu-mode-h (&rest _)
                (if lsp-bridge-mode
                    (corfu-mode -1)
                  (when (and (boundp 'corfu-enable-mode-list)
                             (apply #'derived-mode-p corfu-enable-mode-list))
                    (corfu-mode +1))))))

  ;; VSCode-style key bindings
  (bind-keys :map lsp-bridge-mode-map
             ("<f12>" . lsp-bridge-find-def)
             ("S-<f12>" . lsp-bridge-find-def-return)
             ("<f2>" . lsp-bridge-rename)
             ("M-F" . lsp-bridge-code-format))
  ;; Conflict with my `upcase-previous-word'.
  (keymap-unset acm-mode-map "M-u" t)

  :init
  (celeste/prepare-package lsp-bridge)
  (celeste/prepare-package markdown-mode)

  )

(use-package eglot
  :bind (:map eglot-mode-map
              ("M-F" . eglot-format-buffer))
  :config
  (setq eglot-autoshutdown t)

  (setq eglot-stay-out-of '(flymake))

  (setq eglot-ignored-server-capabilities
        '(:colorProvider
          :foldingRangeProvider))

  ;; Enable `corfu-mode', since `eglot' kindly extends `completion-at-point'.
  (with-eval-after-load 'corfu
    (add-to-list 'eglot-managed-mode-hook #'corfu-mode))

  )

(use-package flycheck-eglot
  :after eglot
  :init (celeste/prepare-package flycheck-eglot)
  :commands flycheck-eglot-mode
  :config
  ;; If `flycheck-eglot-mode' is enabled, then other checkers will be disabled.
  ;; `flycheck-eglot' implements this by set buffer-local `flycheck-checker' to
  ;; `elgot-check', so flycheck won't automatically select checkers in
  ;; `flycheck-checkers'.
  (setq-default flycheck-eglot-exclusive t))

(use-package eglot-tempel
  :after eglot
  :demand t
  :init (celeste/prepare-package eglot-tempel)
  :config
  (eglot-tempel-mode +1))

(use-package ide-zero
  :init (celeste/prepare-package ide-zero)
  :commands ide-zero-define)


;;; Programming Languages.

;; Markup.
(require 'init-yaml)
(require 'init-markdown)
(require 'init-json)

;; General-purpose.
(require 'init-go)
(require 'init-lua)
(require 'init-cc)
(require 'init-python)

;; DSL.
(require 'init-cmake)
(require 'init-protobuf)
(require 'init-sh)

;;; lang/init.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; no-byte-compile: t
;; End:
