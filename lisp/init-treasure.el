;;; init-treasure.el -- Treasures for Emacs users. -*- lexical-binding: t -*-
;;; Commentary:

;; Gifts from two gods: minad and oantolin. These two geniuses provides a lot of
;; useful packages that are: 1) lightweight, with ~1000 LOC each package, 2)
;; reusable, integrated seamlessly with builtin features and other packages, 3)
;; well-documented and easy to customize, compared to those packages with
;; complex functionalities and rather bloat documentation (I mean, yes,
;; straight.el). My huge thanks for these two!

;; I include their most generic, helpful and well-designed packages here. The
;; common of these packages is that they provide some completion
;; functionalities, which are very convenient.

;; Package list:

;; https://github.com/minad/vertico
;; https://github.com/minad/marginalia
;; https://github.com/minad/consult
;; https://github.com/oantolin/orderless
;; https://github.com/oantolin/embark

;;; Code:



(celeste/prepare-package compat)
(celeste/prepare-package vertico "" "extensions")
(celeste/prepare-package marginalia)
(celeste/prepare-package consult)
(celeste/prepare-package orderless)
(celeste/prepare-package embark)


;;; VERTical Interactive COmpletion

(use-package vertico
  :hook ((after-init . vertico-mode))
  :config
  ;; Cycle candidates.
  (setq vertico-cycle t))

;; DEL and M-DEL will delete a part of the path (divided by /) when possible.
(use-package vertico-directory
  :after rfn-eshadow
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
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

;; Use "M-B" to toggle `vertico-buffer-mode'.
(use-package vertico-buffer
  :commands vertico-buffer-mode
  :config
  (setq vertico-buffer-display-action
        '(display-buffer-use-least-recent-window)))


;;; Marginalia (n. marginal notes) in the minibuffer.

(use-package marginalia
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

  :init (bind-keys ("C-c s c" . +consult-emacs-configurations))

  :bind (("C-c s ." . consult-recent-file)
         ("C-c s f" . consult-fd)
         ("C-c s o" . consult-outline)
         ("C-c s b" . consult-buffer)
         ("C-c s g" . consult-ripgrep)
         ("C-c s m" . consult-bookmark)
         ("C-c s a" . consult-man)      ; apropos
         ("M-g g" . consult-goto-line)
         ([remap yank-pop] . consult-yank-pop)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap project-switch-to-buffer] . consult-project-buffer))

  :config
  ;; https://github.com/minad/consult/wiki#manual-preview-for-non-consult-commands-using-embark
  (progn
    (keymap-set minibuffer-local-map "M-." #'+minibuffer-embark-preview)
    (defun +minibuffer-embark-preview ()
      "Previews candidate in vertico buffer, unless it's a consult command"
      (interactive)
      (unless (bound-and-true-p consult--preview-function)
        (save-selected-window
          (let ((embark-quit-after-action nil))
            (embark-dwim)))))
    )

  ;; https://github.com/minad/consult/wiki#toggle-preview-during-active-completion-session
  (progn
    (defvar-local consult-toggle-preview-orig nil)

    (defun consult-toggle-preview ()
      "Command to enable/disable preview."
      (interactive)
      (if consult-toggle-preview-orig
          (setq consult--preview-function consult-toggle-preview-orig
                consult-toggle-preview-orig nil)
        (setq consult-toggle-preview-orig consult--preview-function
              consult--preview-function #'ignore)))
    (define-key vertico-map (kbd "M-,") #'consult-toggle-preview)
    )

  ;; https://github.com/minad/consult/wiki#previewing-files-in-find-file
  (progn
    (defun +consult-read-file-name (prompt &optional dir default mustmatch initial pred)
      (let ((default-directory (or dir default-directory))
            (minibuffer-completing-file-name t))
        (consult--read #'read-file-name-internal :state (consult--file-preview)
                       :prompt prompt
                       :initial initial
                       :require-match mustmatch
                       :predicate pred)))

    (defun +consult-find-file-maybe-with-preview (arg)
      "`find-file' with consult preview. Only for interactive call"
      (interactive "P")
      (if arg
          (let ((read-file-name-function #'+consult-read-file-name))
            (call-interactively #'find-file))
        (call-interactively #'find-file)))

    (keymap-global-set "C-x C-f" #'+consult-find-file-maybe-with-preview)
    )

  )

(use-package consult-info
  :bind ("C-c s i" . consult-info))

(use-package consult-register
  :bind ("C-c s r" . consult-register))

(use-package consult-kmacro
  :bind ("C-c s k" . consult-kmacro))

;; `consult-org-heading' and `consult-org-agenda'
;; Alternatives for `org-goto'.
(use-package consult-org
  ;; This package is included in consult, so no need for another load-path.
  :after org
  :bind (:map org-mode-map
              ("C-c s h" . consult-org-heading)))

;; Xref integration. No xref UI now!
(use-package consult-xref
  :after xref
  :autoload consult-xref
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


;;; Really interesting and practical completion style.

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic))
  ;; The ampersand affix searches annotation, which is very, very slow.
  (setq orderless-affix-dispatch-alist
        (seq-filter (lambda (affix) (not (eq (car affix) ?&)))
                    orderless-affix-dispatch-alist))

  ;; Fix consult, sophisticated completion style, etc.
  ;; Shipped from https://github.com/minad/consult/wiki#minads-orderless-configuration.

  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default. As a result, "tdoe"
  ;; will firstly try to match `toggle-debug-on-error', "orn" will match
  ;; `org-roam-node-*'. This completion style is more efficient, especially
  ;; when you're already familiar with some frequently-used commands.
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless basic)
        ;; These are default values for `completion-category-overrides', which
        ;; will override the default `completion-styles'. I just want to use
        ;; orderless exclusively.
        completion-category-defaults nil
        completion-category-overrides '(;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism))
                                        ;; REVIEW: 2024-05-29 we need to use the styles provided by eglot
                                        ;; (eglot (styles orderless))
                                        ;; (eglot-capf (styles orderless))
                                        )
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch))
  )


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

  (setq help-char "?")

  ;; When a prefix is typed, type `help-char' ("C-h").
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  (setq embark-help-key help-char)
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
