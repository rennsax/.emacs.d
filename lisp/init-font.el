;;; init-font.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Customizable variables.

(defcustom celeste-other-font-mode-list
  nil
  "List of modes that should use another font."
  :group 'celeste
  :type '(repeat symbol)
  :set (celeste--mode-list-setter celeste/buffer-set-other-font))

(defcustom celeste-default-font-name "MonaspiceRn Nerd Font"
  "The default font of Celeste Emacs. Pick whatever you like."
  :group 'celeste
  :type 'string)

(defcustom celeste-nerd-icon-font-name "MonaspiceRn Nerd Font"
  "The font for displaying nerd icons.

Go to https://www.nerdfonts.com and get a Nerd font."
  :group 'celeste
  :type 'string)

(defcustom celeste-other-font-name "MonaspiceAr Nerd Font"
  "Another font for Celeste Emacs.

I suggest to use a relatively-standard font for it. This font is
mainly used in some transient buffers for reading purpose."
  :group 'celeste
  :type 'string)

(defcustom celeste-cjk-font-name "LXGW WenKai Mono"
  "CJK font for Celeste Emacs.

This CJK font may be used *everywhere* for CJK fonts. By running
the hook `celeste-buffer-face-mode-hook', font for CJK characters
are automatically patched whenever `buffer-face-mode' is enabled.

Generally you can see different fonts in a buffer, and this may
cause problems when you try to make some alignment operations,
such as beautify the tables in `org-mode'. For me, I just prefer
a nice CJK fonts, which constantly pleases me when I'm writing a
Chinese document."
  :group 'celeste
  :type 'string)

(defcustom celeste-really-mono-font-name "Sarasa Term SC Nerd"
  "Really nonospace font.

Ideal: Latin characters has exactly half the width of CJK
characters. I know one font that achieve this, and it's the
default value."
  :group 'celeste
  :type 'string)

(defcustom celeste-font-size 14
  "Font size of Celeste Emacs."
  :group 'celeste
  :type 'number)

(defcustom celeste-buffer-face-mode-hook nil
  "Functions to run after enable `buffer-face-mode'.

Typically, Celeste uses `buffer-face-mode' to enable different
fonts to distinguish buffer context (writing, programming,
poem,...)."
  :group 'celeste
  :type 'hook
  :risky t)


;;; Font settings.
;; Useful hooks:
;; `after-init-hook': not helpful in daemon mode.
;; `server-after-make-frame-hook': triggered when the daemon create a new frame.
;; `text-scale-mode-hook': TODO
;; `after-setting-font-hook': after the frame font is *changed*.

(create-fontset-from-fontset-spec
 (format  "-*-%s-regular-normal-normal-*-%d-*-*-*-p-0-fontset-celeste"
          celeste-default-font-name celeste-font-size))

;; This workaround is found at https://emacs-china.org/t/doom-emacs/23513
;; See the variable `char-script-table'.
(defun +fontset-setup-cjk (&optional fontset)
  "Setup special CJK fonts for FONTSET."
  (interactive)
  (dolist (charset '(kana han cjk-misc bopomofo symbol))
    (set-fontset-font fontset charset (font-spec :family celeste-cjk-font-name))))

;; Patch the fontset after `buffer-face-mode' is enabled.
(add-hook 'celeste-buffer-face-mode-hook #'+fontset-setup-cjk)

;; `after-setting-font-hook' isn't triggered since the font of the initial frame
;; is never *changed*.
(+fontset-setup-cjk "fontset-celeste")

(push '(font . "fontset-celeste") default-frame-alist)


;;; Magical multi-font settings.

;; Set different fonts for those special modes, so I can distinguish from
;; different contexts.
;; REVIEW: it's interesting that Emacs can find `celeste/buffer-set-other-font',
;; even it's defined later.
(setopt celeste-other-font-mode-list
        '(eshell-mode
          magit-mode
          debugger-mode
          Custom-mode
          dired-mode
          org-mode
          org-agenda-mode
          markdown-mode
          Info-mode
          man-common
          help-mode))

(defun celeste/buffer-set-other-font (&optional font-family no-hook)
  "Setup another font for the current buffer.

If FONT-FAMILY is non-nil, use the specified font. Otherwise,
`celeste-other-font-name' is used.

If NO-HOOK is non-nil, by passing the execution of
`celeste-buffer-face-mode-hook'."
  (interactive (list (completing-read (format-prompt "Font family" celeste-other-font-name)
                                      (font-family-list))))
  (let ((font-family (or font-family celeste-other-font-name)))
    (if (string= font-family celeste-default-font-name)
        (buffer-face-mode -1)
      (progn
        (setq-local buffer-face-mode-face `(:family ,font-family))
        (buffer-face-mode +1)
        (unless no-hook
          (run-hooks 'celeste-buffer-face-mode-hook))))))

(use-package nerd-icons
  :init
  (setq nerd-icons-font-family celeste-nerd-icon-font-name))



(provide 'init-font)
;;; init-font.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
