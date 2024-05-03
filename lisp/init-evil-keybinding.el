;;; init-evil-keybinding.el -- Global key bindings for evil-mode. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-unset global-map "C-x <escape> <escape>")

;; TODO: add a disabled list
(defun +evil-collection-init (module)
  "Initialize evil-collection-MODULE immediately.

MODULE is the mode name (symbol).

The function aims to replace `evil-collection-init' because it
breaks the load order and is unpredictable."
  (evil-collection-init (list module)))

;; BUG: `evil-collection-init' is here.
(ignore-errors
  (evil-collection-init))

(evil-define-key* 'visual 'global
  "*" #'evil-visualstar/begin-search-forward
  "#" #'evil-visualstar/begin-search-backward)

;; Visually move up and down. Useful when (soft) line wrap is enabled.
(evil-define-key* 'normal 'global
  [up]            #'evil-previous-visual-line
  [down]          #'evil-next-visual-line)
;; In insert-state, move the window.
(evil-define-key* nil minibuffer-mode-map
  (kbd "C-u")     #'evil-delete-back-to-indentation)
;; Switch to the recent buffer even in insert-state.
(evil-define-key* 'insert 'global
  (kbd "C-w")     #'evil-window-map
  (kbd "C-6")     #'evil-switch-to-windows-last-buffer)

(defvar celeste-leader-map nil "Leader key prefix maps.")
(define-prefix-command 'celeste-leader-map)

(evil-define-key* '(normal motion) 'global
  (kbd celeste-leader-key)  #'celeste-leader-map)

(defun celeste/add-leader-map (&rest args)
  "Add leader map. ARGS are passed to `evil-define-key*'."
  (declare (indent defun))
  (apply #'evil-define-key* nil celeste-leader-map args))

(celeste/add-leader-map
  "n"             #'evil-ex-nohighlight
  "rg"            #'deadgrep

  "s."            #'consult-recent-file
  "sf"            #'consult-fd
  "so"            #'consult-outline
  "sd"            #'consult-flycheck ; Search Diagnostic
  "sb"            #'consult-buffer
  "sc"            #'+consult-emacs-configurations
  "sw"            #'consult-ripgrep

  (kbd celeste-leader-key) #'persp-switch-to-buffer*

  "bx"            #'kill-current-buffer
  "bs"            #'scratch-buffer
  "be"            #'eshell
  "bt"            #'vterm

  "oA"            #'org-agenda
  "oa"            #'org-agenda-list
  "orn"           #'org-roam-node-find

  "gg"            #'magit-status
  "gsh"           #'diff-hl-show-hunk
  "gei"           #'magit-gitignore-in-topdir ; [e]dit git[i]gnore
  "gma"           #'magit-submodule-add ; sub[m]odule [a]dd
  "gmr"           #'magit-submodule-remove ; sub[m]odule [r]emove
  "gmc"           #'magit-clone

  "ie"            #'emoji-search
  "iE"            #'emoji-insert

  "zz"            #'writeroom-mode)

(with-eval-after-load 'perspective
  (celeste/add-leader-map "p" #'perspective-map))

;; Move across matched files.
(with-eval-after-load 'deadgrep
  (evil-define-key* 'normal deadgrep-mode-map
    (kbd "C-k") #'deadgrep-backward-filename
    (kbd "C-j") #'deadgrep-forward-filename))

(with-eval-after-load 'org
  (evil-define-key* 'normal org-mode-map
    (kbd "<return>") #'+org/dwim-at-point))

(with-eval-after-load 'image-mode
  (evil-define-key* 'normal image-mode-map
    "n" #'image-next-file
    "p" #'image-previous-file))

(with-eval-after-load 'doc-view
  (evil-define-key* 'normal doc-view-mode-map
    "n" #'doc-view-next-page
    "p" #'doc-view-previous-page))

;; C-w is mapped to `vterm--self-insert' by `evil-collection'
(with-eval-after-load 'vterm
    (evil-define-key* 'insert vterm-mode-map
      (kbd "C-w") #'evil-window-map))

(add-hook 'eshell-first-time-mode-hook
  (defun +eshell-setup-keys ()
    ;; TODO Shell-like C-d
    (evil-define-key* 'insert eshell-mode-map
      (kbd "C-p") #'eshell-previous-input
      (kbd "C-n") #'eshell-next-input
      (kbd "C-u") #'+eshell-kill-whole-input
      (kbd "C-a") #'+eshell-input-bol)))

(with-eval-after-load 'dired
  (evil-define-key* 'normal dired-mode-map
    (kbd "s-r") #'revert-buffer))

(with-eval-after-load 'org-goto
  ;; `org-goto' is a convenient way to navigate an org buffer. However, it
  ;; conflicts with `evil-mode'.
  (advice-add 'org-goto :around
              (lambda (oldfun &rest r)
                (evil-with-state emacs (apply oldfun r)))))

(with-eval-after-load 'gptel
  ;; Questions have leading "###", so it's useful to travel around the same level.
  (evil-define-key* 'normal gptel-mode-map
    "]]" #'markdown-forward-same-level
    "[[" #'markdown-backward-same-level))

(with-eval-after-load 'lsp-bridge
  (evil-define-key* 'normal lsp-bridge-mode-map
    "gd" #'lsp-bridge-find-def
    (kbd "<f2>")  #'lsp-bridge-rename
    ))

(with-eval-after-load 'mwim
  (evil-define-key 'insert 'global
    ;; BOL or indent.
    (kbd "C-a") #'mwim-beginning
    ;; Skip comments.
    (kbd "C-e") #'mwim-end))

(provide 'init-evil-keybinding)
;;; init-evil-keybinding.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End: