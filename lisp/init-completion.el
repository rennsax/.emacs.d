;;; init-completion.el -- Completion setups. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-package))

;; VERTical Interactive COmpletion
(celeste/use-package vertico
  :hook ((after-init . vertico-mode))
  :defines (vertico-map)
  :config
  ;; Vertico extensions
  (eval-and-compile
    (add-to-list 'load-path (concat celeste-package-dir "vertico/extensions/")))

  ;; DEL and M-DEL will delete a part of the path (divided by /) when possible.
  (use-package vertico-directory
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; TODO: what's `rfn-eshadow-update-overlay'?
    :bind (:map vertico-map
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word)))

  (use-package vertico-multiform
    :hook (vertico-mode . vertico-multiform-mode)
    :config

    ;; Use grid view for `jinx-correct' completion menu. Recommended by minad himself.
    (require 'vertico-grid)
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))

    ;; From https://github.com/minad/vertico/wiki#candidate-display-transformations-custom-candidate-highlighting
    ;; Show different colors for directories/enabled modes in vertico.
    (defvar +vertico-transform-functions nil)

    (cl-defmethod vertico--format-candidate :around
      (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
      (dolist (fun (ensure-list +vertico-transform-functions))
        (setq cand (funcall fun cand)))
      (cl-call-next-method cand prefix suffix index start))

    (defun +vertico-highlight-directory (file)
      "If FILE ends with a slash, highlight it as a directory."
      (when (string-suffix-p "/" file)
        (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
      file)

    (defun +vertico-highlight-enabled-mode (cmd)
      "If MODE is enabled, highlight it as font-lock-constant-face."
      (let ((sym (intern cmd)))
        (with-current-buffer (nth 1 (buffer-list))
          (if (or (eq sym major-mode)
                  (and
                   (memq sym minor-mode-list)
                   (boundp sym)
                   (symbol-value sym)))
              (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
        cmd))

    (add-to-list 'vertico-multiform-categories
                 '(file
                   (+vertico-transform-functions . +vertico-highlight-directory)))
    (add-to-list 'vertico-multiform-commands
                 '(execute-extended-command
                   (+vertico-transform-functions . +vertico-highlight-enabled-mode))))
  )

;; Marginalia (n. marginal notes) in the minibuffer.
(celeste/use-package marginalia
  :hook (after-init . marginalia-mode))

;; Like Telescope in Neovim.
(celeste/use-package consult
  :demand t
  :commands consult-fd
  :config
  (defun +consult-emacs-configurations ()
    "Search Emacs configurations files."
    (interactive)
    (consult-fd user-emacs-directory "lisp/"))
  (use-package consult-info
    :init
    (defun consult-info-emacs ()
      "Search through Emacs info pages."
      (interactive)
      (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

    (defun consult-info-org ()
      "Search through the Org info page."
      (interactive)
      (consult-info "org"))

    (defun consult-info-completion ()
      "Search through completion info pages."
      (interactive)
      (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                    "corfu" "cape" "tempel"))
    :commands consult-info)

  (eval-when-compile
    (celeste/use-package flycheck))
  (celeste/use-package consult-flycheck
    :after flycheck
    :commands consult-flycheck))

;; Lightweight completion engine.
(celeste/use-package corfu
  ;; Replaced by acm in lsp-bridge
  :disabled t
  :commands corfu-quit
  :config
  (setq corfu-auto t
        corfu-auto-prefix 3
        ;; Do not do anything (such as the default "insert") on exact match. So
        ;; a manual TAB is always needed for completion.
        corfu-on-exact-match nil
        )
  ;; Disable RET -> `corfu-insert'. I prefer primitive RET - just `newline'.
  (keymap-unset corfu-map "RET")

  ;; Use "C-SPC" to toggle corfu completion popup.
  (keymap-set global-map "C-SPC" #'completion-at-point)
  (keymap-set corfu-map "C-SPC" #'corfu-quit)

  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  ;; Toggle corfu in all buffers
  :hook ((after-init . global-corfu-mode)
         ;; In `eshell-mode', do not automatically toggle corfu prompt.
         (eshell-mode . (lambda ()
                          (setq-local corfu-auto nil)))))


;; Fuzzy finder
(celeste/use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic)))

;; Minibuffer actions. Use "C-;" to ease your life!
(celeste/use-package embark
  :bind (("C-;" . embark-act))
  :commands embark-prefix-help-command
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Lightweight templates.
(celeste/use-package tempel
  :preface
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))
    (tempel-abbrev-mode))
  :hook ((conf-mode prog-mode text-mode) . tempel-setup-capf)
  :commands (tempel-expand tempel-abbrev-mode tempel-complete)
  :config
  (setq tempel-trigger-prefix "<t<")
  )


(provide 'init-completion)
;;; init-completion.el ends here
