;;; init-completion.el -- Completion setups. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; VERTical Interactive COmpletion
(celeste/use-package vertico
  :hook ((after-init . vertico-mode))
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


;;; consult: fuzzy finder in Emacs

(add-to-list 'load-path (concat celeste-package-dir "consult"))
(use-package consult
  :bind (("C-c s ." . consult-recent-file)
         ("C-c s f" . consult-fd)
         ("C-c s o" . consult-outline)
         ("C-c s b" . consult-buffer)
         ("M-y" . consult-yank-pop))
  :preface
  (defun +consult-emacs-configurations ()
    "Search Emacs configurations files."
    (interactive)
    (consult-fd user-emacs-directory "lisp/"))
  :init
  (bind-key "C-c s c" #'+consult-emacs-configurations))

;; `consult-org-heading' and `consult-org-agenda'
;; Alternatives for `org-goto'.
(use-package consult-org
  ;; This package is included in consult, so no need for another load-path.
  :after org
  :bind (:map org-mode-map
              ("C-c s h" . consult-org-heading)))


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
