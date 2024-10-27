;;; init-hydra.el -- Hydra: versatile key binding system. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(celeste/prepare-package hydra)
(require 'hydra)

;;; Memorandum:

;; | color    | toggle                     |
;; |----------+----------------------------|
;; | red      |                            |
;; | blue     | :exit t                    |
;; | amaranth | :foreign-keys warn         |
;; | teal     | :foreign-keys warn :exit t |
;; | pink     | :foreign-keys run          |


;;; Consult
;; Benefits: show prompts like whihc-key; cancel the command immediately,
;; without the third key. Problem: cannot map `org-mode-map' individually.

(unless t
  (defhydra hydra-consult (:color blue) ; blue: quit immediately
    "consult"
    ("b" consult-buffer "buffer")
    ("." consult-recent-file "recent-file")
    ("c" +consult-emacs-configurations "emacs configurations")
    ("f" consult-fd "fd")
    ("o" consult-outline "outline")
    ("g" consult-ripgrep "ripgrep")
    ("m" consult-bookmark "bookmark")
    ("a" consult-man "man")
    ("r" consukt-register "register")
    ("i" consult-info "info")
    ("k" consult-kmacro "kmacro")
    ("d" consult-flycheck "flycheck (diagnosis)")
    ("h" consult-org-heading "org heading"))

  (keymap-global-set "C-c s" #'hydra-consult/body))


;;; VIM

(defhydra hydra-vi (:color amaranth)
  "VIM-style navigation"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("d" scroll-up-half)
  ("u" scroll-down-half)
  ("f" scroll-up)
  ("b" scroll-down)
  ("z z" recenter)
  ("z t" (let ((this-scroll-margin
                (min (max 0 scroll-margin)
                     (truncate (/ (window-body-height) 4.0)))))
           (recenter this-scroll-margin t)))
  ("z b" (let ((this-scroll-margin
                (min (max 0 scroll-margin)
                     (truncate (/ (window-body-height) 4.0)))))
           (recenter (- -1 this-scroll-margin) t)))
  ("n" scroll-up-line)
  ("p" scroll-down-line)
  ("q" nil)
  ("C-z" nil))
(hydra-set-property 'hydra-vi :verbosity 1)
(keymap-global-set "C-z" #'hydra-vi/body)


;;; Window management.

(defhydra hydra-window-resize ()
  ("w" enlarge-window-horizontally "widen")
  ("n" shrink-window-horizontally "narrow")
  ("h" enlarge-window "heighten")
  ("s" shrink-window "shrink")
  ("q" nil "quit"))

(keymap-global-set "C-M-w" #'hydra-window-resize/body)

(defhydra hydra-window (:pre (when (fboundp 'sis-set-english)
                               (sis-set-english)))
  "window"
  ("h" windmove-left)
  ("l" windmove-right)
  ("k" windmove-up)
  ("j" windmove-down)

  ("u" winner-undo)
  ("C-r" winner-redo)

  ("v" (lambda () (interactive) (split-window-right) (windmove-right)) "vert")
  ("x" (lambda () (interactive) (split-window-below) (windmove-down)) "horz")

  ("a" ace-window "ace")
  ("d" ace-delete-window "del")
  ("D" delete-window "del this")
  ("s" ace-swap-window "swap")
  ("o" delete-other-windows "only")

  ("b" consult-buffer "buf")
  ("p b" consult-project-buffer "proj-buf")
  ("." consult-recent-file "recf")

  ("r" rotate-layout "rot")             ; from rotate
  ("+" balance-windows "bal")

  ("c" ace-copy-window "copy")

  ("C-g" nil)
  ("q" nil "quit"))

(keymap-global-set "M-o" #'hydra-window/body)


(defhydra hydra-org-speed-commands (:hint nil :color red)
  "
^Outline^          ^Visibility^      ^Editing^ ^^            ^Metadata^
--------------------------------------------------------------
_n_ext             _c_: cycle        _U_p      _#_: comment  _:_: tag
_p_revious         _C_: shifttab     _D_own    _@_: mark     _P_roperty
_f_orward (same)   _<SPC>_: path     _r_ight   _\\^_: sort
_b_ackward (same)  _s_: narrow       _l_eft
_u_p               _k_: cut          _R_metaR
^^                 _=_: column       _L_metaL
"
  ("n" org-next-visible-heading)
  ("p" org-previous-visible-heading)
  ("f" org-forward-heading-same-level)
  ("b" org-backward-heading-same-level)
  ("u" outline-up-heading)

  ("c" org-cycle)
  ("C" org-shifttab)
  ("<TAB>" org-cycle)
  ("<backtab>" org-shifttab)

  ("<SPC>" org-display-outline-path)
  ("s" org-toggle-narrow-to-subtree)
  ("k" org-cut-subtree)
  ("=" org-columns)

  ("U" org-metaup)
  ("D" org-metadown)
  ("r" org-metaright)
  ("l" org-metaleft)
  ("R" org-shiftmetaright)
  ("L" org-shiftmetaleft)
  ("^" org-sort)
  ("@" org-mark-subtree)
  ("#" org-toggle-comment)

  (":" org-set-tags-command)
  ("P" org-set-property)

  ("q" nil "quit"))

(with-eval-after-load 'org
  (keymap-set org-mode-map "C-." #'hydra-org-speed-commands/body))


;;; Toggle rarely-used modes.

(defvar whitespace-mode nil)
(defhydra hydra-toggle (:color red :hint nil)
  "
[_a_]bbrev-mode:       %`abbrev-mode         [_r_]edraw-display
[_d_]ebug-on-error:    %`debug-on-error
auto-[_f_]ill-mode:    %`auto-fill-function
[_t_]runcate-lines:    %`truncate-lines
[_w_]hitespace-mode:   %`whitespace-mode
[_o_]verwrite-mode:    %`overwrite-mode

"
  ("a" abbrev-mode)
  ("d" toggle-debug-on-error)
  ("f" auto-fill-mode)
  ("t" toggle-truncate-lines)
  ("w" whitespace-mode)
  ("o" overwrite-mode)
  ("r" redraw-display :color blue)
  ("q" nil "quit"))

(keymap-global-set "S-<f6>" #'hydra-toggle/body)


(provide 'init-hydra)
;;; init-hydra.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
