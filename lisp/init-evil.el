;;; init-evil.el -- Evil: dark side of Emacs. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

(require 'init-package)

;; Silence `evil-collection' warnings.
(eval-when-compile
  (celeste/require 'annalist)
  (defvar evil-want-keybinding nil))

(celeste/use-package evil
  :demand t
  :commands (evil-emacs-state)
  ;; TODO :when custom

  ;; Toggle "Evil-Local mode" mode in *all* buffers.
  :hook (after-init . evil-mode)

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
        evil-undo-system 'undo-redo
        ;; PERF: Stop copying the selection to the clipboard each time the cursor
        ;; moves in visual mode.
        evil-visual-update-x-selection-p nil
        ;; Use the search module `evil-search', instead of `isearch'.
        evil-search-module 'evil-search)

  :config
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
  (keymap-set evil-insert-state-map "<escape>" 'evil-normal-state)

  ;; Also use Emacs's style bindings in ":" command line.
  (keymap-unset evil-command-line-map "C-f")
  (keymap-set evil-command-line-map "C-b" #'backward-char)

  ;; BUG: if begins from the first line, there is something wrong.
  ;; Keep the current line at the center of the screen while page down/up.
  ;; This implements the same behavior of "noremap <c-d> <c-d>zz" in Vim.
  ;; (defun +evil-scroll-to-center-a (&rest _)
  ;;   (evil-scroll-line-to-center nil))
  ;; (advice-add #'evil-scroll-down :after #'+evil-scroll-to-center-a)
  ;; (advice-add #'evil-scroll-up :after #'+evil-scroll-to-center-a)
  )

;;; Evil extensions

;; A bunch of community evil key bindings => use evil almost everywhere!
(celeste/use-package evil-collection
  :commands evil-collection-init
  :config
  ;; Protect some keys - they should never be mapped!
  (setq evil-collection-key-blacklist
        (append (list celeste-leader-key)
                '("[" "]" "<escape>")
                ;; `evil-replace-with-register'
                '("g r"))))

;; Comment line/block with `gc'!
(celeste/use-package evil-nerd-commenter
  :bind (:map evil-normal-state-map
	 ("g c" . evilnc-comment-operator)
	 :map evil-visual-state-map
	 ("g c" . evilnc-comment-operator))
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

;; Port of vim.surround.
(celeste/use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

;; * and # in `evil-visual-state' search the selected pattern.
(celeste/use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward))

;; Provide "x" text object to manipulate html/xml tag attributes
(celeste/use-package exato
  :after evil
  ;; TODO It's not easy to lazy-load a textobject plugin.
  :demand t)

;; Show search result counts.
(celeste/use-package anzu)
(celeste/use-package evil-anzu
  :after evil
  :demand t
  :diminish
  :config
  (add-hook 'evil-mode-hook 'global-anzu-mode))

(celeste/use-package evil-replace-with-register
  :hook (evil-mode . evil-replace-with-register-install)
  :config
  (setq evil-replace-with-register-key (kbd "gr")))

;; Display visual hint on evil edit operations. Even better than Vim!
(celeste/use-package evil-goggles
  :hook (evil-mode . evil-goggles-mode)
  :init
  (setq evil-goggles-enable-delete nil
        evil-goggles-enable-change nil
        evil-goggles-enable-nerd-commenter nil)
  :config
  (setq evil-goggles-duration 0.3)
  )


(provide 'init-evil)
;;; init-evil.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
