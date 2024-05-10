;;; init-treasure.el -- Treasures for Emacs users. -*- lexical-binding: t -*-
;;; Commentary:

;; Gifts from two gods: minad and oantolin. These two geniuses provides a lot of
;; useful packages that are: 1) lightweight, with ~1000 LOC each package, 2)
;; reusable, integrated seamlessly with builtin features and other packages, 3)
;; well-documented and easy to customize, compared to those packages with
;; complex functionalities and rather bloat documentation (I mean, yes,
;; straight.el). My huge thanks for these two!

;; I include their most generic, helpful and well-designed packages here.

;; Package list:

;; https://github.com/minad/vertico
;; https://github.com/minad/marginalia
;; https://github.com/minad/consult
;; https://github.com/oantolin/orderless
;; https://github.com/oantolin/embark

;;; Code:



(celeste/use-package vertico)
(celeste/use-package marginalia)
(celeste/use-package consult)
(celeste/use-package orderless)
(celeste/use-package embark)


;;; VERTical Interactive COmpletion

(use-package vertico
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


;;; Marginalia (n. marginal notes) in the minibuffer.

(celeste/use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))


;;; consult: fuzzy finder in Emacs

(use-package consult
  :preface
  (defun +consult-emacs-configurations ()
    "Search Emacs configurations files."
    (interactive)
    (consult-fd user-emacs-directory "lisp/"))
  (defun +consult-buffer-maybe-other-window (arg)
    "If prefix ARG is non-nil, use another window."
    (interactive "P")
    (if arg
        (call-interactively #'consult-buffer-other-window)
      (call-interactively #'consult-buffer)))

  :init (bind-keys ("C-x b" . +consult-buffer-maybe-other-window) ; alternate `switch-to-buffer'
                   ("C-c s c" . +consult-emacs-configurations))
  :bind (("C-c s ." . consult-recent-file)
         ("C-c s f" . consult-fd)
         ("C-c s o" . consult-outline)
         ("C-c s b" . consult-buffer)
         ("C-c s g" . consult-ripgrep)
         ("C-c s m" . consult-bookmark)
         ("M-y" . consult-yank-pop)     ; alternate `yank-pop'
         :map project-prefix-map
         ("b" . consult-project-buffer) ; alternate `project-switch-to-buffer'
         )

  :commands consult-buffer-other-window
  :config
  ;; Filtered buffers are still available in hidden buffers (with SPC switch).
  (setq consult-buffer-filter
        (append consult-buffer-filter
                '("\\*helpful.*\\*" "\\*Help\\*")))
  (with-eval-after-load 'popper
    ;; Buffers managed by popper are also boring.
    (cl-loop for entry in popper-reference-buffers
             when (stringp entry) do (add-to-list 'consult-buffer-filter entry)))
  )

;; `consult-org-heading' and `consult-org-agenda'
;; Alternatives for `org-goto'.
(use-package consult-org
  ;; This package is included in consult, so no need for another load-path.
  :after org
  :bind (:map org-mode-map
              ("C-c s h" . consult-org-heading)))



;;; Really interesting and practical completion style.

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic))
  ;; The ampersand affix searches annotation, which is very, very slow.
  (setq orderless-affix-dispatch-alist
        (seq-filter (lambda (affix) (not (eq (car affix) ?&)))
                    orderless-affix-dispatch-alist)))


;;; Use "C-;" to ease your life!

(use-package embark
  :bind (("C-;" . embark-act)
         ("C-h B" . embark-bindings)
         ;; Sensible, because the default actions for these kinds of targets are
         ;; almost equivalent to `xref-find-definitions'.
         ("M-." . embark-dwim)
         ("s-K" . +embark-kill-this-buffer-and-window)
         :map minibuffer-mode-map
         ;; Sometimes I don't want to close the minibuffer after doing embark
         ;; actions. Or, use "q" after `embark-act' to toggle.
         ("C-," . embark-act-noquit))
  :commands (embark-prefix-help-command
             embark-kill-buffer-and-window)
  :preface
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))
  (defun +embark-kill-this-buffer-and-window ()
    "Kill current buffer and delete its window."
    (interactive)
    (embark-kill-buffer-and-window (current-buffer)))
  :init

  ;; Embark makes this option really meaningful!
  (setq enable-recursive-minibuffers t)

  ;; When a prefix is typed, type `help-char' ("C-h").
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  (setq help-char "?")
  (setq embark-help-key "?")

  (setq embark-indicators
        '(embark-minimal-indicator  ; default is embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  ;; TODO: what does `embark--vertico-indicator' actually does? It's auto-added
  ;; to `embark-indicators' after vertico is loaded. Related issue:
  ;; https://github.com/oantolin/embark/issues/427
  ; (setcar embark-indicators #'embark-minimal-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Display the Embark keybinding buffer in a different vertico form.
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

  ;; Show the current possible embark target, similar to `which-function-mode'.
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#show-the-current-embark-target-types-in-the-modeline
  (progn
    (defvar embark--target-mode-timer nil)
    (defvar embark--target-mode-string "")

    (defun embark--target-mode-update ()
      (setq embark--target-mode-string
            (if-let (targets (embark--targets))
                (format "[%s%s] "
                        (propertize (symbol-name (plist-get (car targets) :type)) 'face 'bold)
                        (mapconcat (lambda (x) (format ", %s" (plist-get x :type)))
                                   (cdr targets)
                                   ""))
              "")))

    (define-minor-mode embark-target-mode
      "Shows the current targets in the modeline."
      :global t
      (setq mode-line-misc-info (assq-delete-all 'embark-target-mode mode-line-misc-info))
      (when embark--target-mode-timer
        (cancel-timer embark--target-mode-timer)
        (setq embark--target-mode-timer nil))
      (when embark-target-mode
        (push '(embark-target-mode (:eval embark--target-mode-string)) mode-line-misc-info)
        (setq embark--target-mode-timer
              (run-with-idle-timer 0.1 t #'embark--target-mode-update))))
    )
  )

;; This feature is auto-loaded after org and embark is loaded.
(use-package embark-org
  :config
  (advice-add 'embark-org-copy-as-markdown :before (lambda (&rest _) (require 'ox-md)))
  )

;; Embark loads it after consult if found.
(use-package embark-consult
  :hook ((embark-collect-mode . consult-preview-at-point-mode)))



(provide 'init-treasure)
;;; init-treasure.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End: