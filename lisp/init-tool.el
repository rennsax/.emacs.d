;;; init-tool.el -- Tools integration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Yet another great "ripgrep" frontend!
(celeste/use-package deadgrep
  :init
  ;; Dep: Fancy progress-bars in mode-line.
  (celeste/use-package spinner)
  :bind ("C-c r g" . deadgrep))

;; Enchanted spell checker.
(celeste/use-package jinx
  :diminish
  :commands jinx-mode
  :config
  (setq jinx-languages "en_US")
  ;; Exclude Chinese characters. This should be a universal setting, so I put it
  ;; here (instead of init-cjk.el)
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))

  (with-eval-after-load 'vertico-multiform
    ;; Use grid view for `jinx-correct' completion menu. Recommended by minad himself.
    (require 'vertico-grid)
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))
    )
  (keymap-global-set "M-$" #'jinx-correct)

  :init
  (defcustom jinx-mode-dict-alist
    '((emacs-lisp-mode ("el"))
      (python-mode ("numpy"))
      (go-mode ("fmt")))
    "Mode-local dictionaries."
    :group 'jinx
    ;; TODO type?
    :type '(list (symbol (list string))))

  (defcustom jinx-enable-mode-list
    '(prog-mode text-mode)
    "Modes that jinx should be enabled."
    :group 'jinx
    :type '(list (symbol)))

  ;; Manually setup mode-local words and enable jinx in specified modes.
  (mapc (lambda (mode-word-list)
          (let ((mode (car mode-word-list))
                (word-list (cadr mode-word-list)))
            (add-hook (intern (concat (symbol-name mode) "-hook"))
                      #'(lambda ()
                          (setq jinx-local-words
                                (mapconcat #'identity word-list " "))
                          (jinx-mode +1)))))
        jinx-mode-dict-alist)
  ;; Must *after* setting `jinx-local-words'.
  (celeste/add-mode-hook jinx-enable-mode-list #'jinx-mode)
  )

;; Use "C-;" to ease your life!
(celeste/use-package embark)

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



(provide 'init-tool)
;;; init-tool.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
