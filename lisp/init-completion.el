;;; init-completion.el -- Completion setups. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


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
  :config
  (defun +consult-emacs-configurations ()
    "Search Emacs configurations files."
    (interactive)
    (consult-fd user-emacs-directory "lisp/"))

  ;; `consult-org-heading' and `consult-org-agenda'
  ;; Alternatives for `org-goto'.
  (with-eval-after-load 'org
    (require 'consult-org))

  ;; `consult-flycheck'
  (with-eval-after-load 'flycheck
    (celeste/use-package consult-flycheck
      :after flycheck
      :commands consult-flycheck)))


;; Fuzzy finder
(celeste/use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))


(provide 'init-completion)
;;; init-completion.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
