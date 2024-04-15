;;; init-evil.el -- Evil: dark side of Emacs. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

(celeste/use-package evil
  :demand t

  ;; Since `evil' is eagerly required, it doesn't matter that we put these
  ;; `setq's in the init section.
  :init
  ;; required by `evil-collection'
  (setq evil-want-keybinding nil
        evil-want-integration t)

  (setq evil-want-C-u-scroll t
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t ; do not extend lines
        evil-symbol-word-search t ; * and # search for words otherwise for symbols
        ;; Only highlight in the selected window.
        evil-ex-interactive-search-highlight 'selected-window
        ;; Innocuous BOL and EOL errors shouldn't abort macro.
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system 'undo-redo ; TODO: is it reliable?
        ;; PERF: Stop copying the selection to the clipboard each time the cursor
        ;; moves in visual mode.
        evil-visual-update-x-selection-p nil
        ;; Use the search module `evil-search', instead of `isearch'.
        evil-search-module 'evil-search)

  :config
  ;; Toggle "Evil-Local mode" mode in *all* buffers.
  (evil-mode)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial
              :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Use VIM-like save message.
  (unless noninteractive
    (setq save-silently t)
    (add-hook 'after-save-hook
      (defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-truename buffer-file-name)
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))

  ;; Sync `evil-shift-width' with `tab-width'.
  (add-hook 'after-change-major-mode-hook
	    (lambda () (setq evil-shift-width tab-width)))

  ;; Use Emacs keybindings in insert state (clear all initial mappings in insert
  ;; state).
  (setcdr evil-insert-state-map nil)
  ;; But keep [escape] as a valid mapping, otherwise we cannot quit insert state
  ;; normally!
  (keymap-set evil-insert-state-map "<escape>" #'evil-normal-state)

  ;; TODO: hello orphan
  (keymap-unset global-map "C-x <escape> <escape>")

  ;; Evil key bindinds!
  (let ((leader-key celeste-leader-key))

    ;; Visually move up and down. Useful when (soft) line wrap is enabled.
    (evil-define-key* 'normal 'global
      [up] #'evil-previous-visual-line
      [down] #'evil-next-visual-line)

    ;; In minibuffer, use "C-u" to clear the line.
    (keymap-set minibuffer-mode-map "C-u" #'evil-delete-back-to-indentation)

    ;; In insert-state, move the window.
    (keymap-set evil-insert-state-map "C-w h" #'evil-window-left)
    (keymap-set evil-insert-state-map "C-w j" #'evil-window-down)
    (keymap-set evil-insert-state-map "C-w k" #'evil-window-up)
    (keymap-set evil-insert-state-map "C-w l" #'evil-window-right)
    (keymap-set evil-insert-state-map "C-w c" #'evil-window-delete)
    (keymap-set evil-insert-state-map "C-w o" #'delete-other-windows)

    ;; Leader mappings.
    (define-prefix-command 'celeste-leader-map)
    (keymap-set evil-motion-state-map leader-key 'celeste-leader-map)
    (keymap-set evil-normal-state-map leader-key 'celeste-leader-map)

    (evil-define-key* nil celeste-leader-map
      "n" #'evil-ex-nohighlight
      "rg" #'deadgrep
      "s." #'consult-recent-file
      "sf" #'consult-fd
      "so" #'consult-outline
      ;; Search Diagnostic
      "sd" #'consult-flycheck

      (kbd celeste-leader-key) #'consult-buffer

      "bx" #'kill-current-buffer
      "bs" #'scratch-buffer
      "be" #'eshell

      "oA" #'org-agenda
      "orn" #'org-roam-node-find

      "gsh" #'diff-hl-show-hunk
      ;; [e]dit git[i]gnore
      "gei" #'magit-gitignore-in-topdir

      "ie" #'emoji-search
      "iE" #'emoji-insert

      "zz" #'writeroom-mode))
  )

;;; Evil extensions

;; A bunch of community evil key bindings => use evil almost everywhere!
(celeste/use-package evil-collection
  :demand t
  :after evil
  :config
  ;; Protect some keys - they should never be mapped!
  (setq evil-collection-key-blacklist
        (append (list celeste-leader-key)
                '("[" "]" "<escape>")
                ;; `evil-replace-with-register'
                '("g r")))
  (evil-collection-init))

;; Comment line/block with `gc'!
(celeste/use-package evil-nerd-commenter
  :bind (:map evil-normal-state-map
	 ("g c" . evilnc-comment-operator)
	 :map evil-visual-state-map
	 ("g c" . evilnc-comment-operator))
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

;; Escape insert state with key sequence.
(celeste/use-package evil-escape
  :demand t
  :after evil
  :init
  (setq evil-escape-key-sequence "jj"
        evil-escape-delay 1.0
        evil-escape-excluded-states '(normal visual multiedit emacs motion))
  :config
  (evil-escape-mode)
  (add-hook 'evil-escape-inhibit-functions
            'minibufferp))

;; Port of vim.surround.
(celeste/use-package evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode))

;; * and # in `evil-visual-state' search the selected pattern.
(celeste/use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

;; Provide "x" text object to manipulate html/xml tag attributes
(celeste/use-package exato
  :after evil
  ;; TODO It's not easy to lazy-load a textobject plugin.
  :demand t)

;; Show search result counts.
(celeste/use-package evil-anzu
  :after evil
  :init
  (celeste/use-package anzu)
  :diminish
  :demand t
  :config
  (global-anzu-mode +1))

(celeste/use-package evil-replace-with-register
  :after evil
  :demand t
  :config
  (setq evil-replace-with-register-key (kbd "gr"))
  (evil-replace-with-register-install))


(provide 'init-evil)
;;; init-evil.el ends here
