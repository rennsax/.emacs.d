;;; init-font.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Customizable variables.

(defcustom celeste-other-font-mode-list
  nil
  "List of modes that should use another font.

Set different fonts for those special modes, so the user can easily switch
between different contexts."
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

(make-obsolete-variable 'celeste-buffer-face-mode-hook nil "2024-09-04")


;;; Font settings.
;; Useful hooks:
;; `after-init-hook': not helpful in daemon mode.
;; `server-after-make-frame-hook': triggered when the daemon create a new frame.
;; `text-scale-mode-hook': TODO
;; `after-setting-font-hook': after the frame font is *changed*.

;; This workaround is found at https://emacs-china.org/t/doom-emacs/23513
;; See the variable `char-script-table'.
(defun celeste--fontset-setup-cjk (&optional fontset)
  "Setup special CJK fonts for FONTSET."
  (dolist (charset '(kana han cjk-misc bopomofo symbol))
    (set-fontset-font fontset charset (font-spec :family celeste-cjk-font-name))))

(defun celeste--create-fontset (alias-prefix)
  "Create a fontset with ALIAS-PREFIX as the prefix of its short name.

Return the alias (short name) of the created fontset.

The short name is formatted as ALIAS-PREFIX_FONT-NAME, where FONT-NAME is
`celeste-default-font-name'.

CJK charsets are specially handled. Their font family is set to
`celeste-cjk-font-name'."
  (when (string-match-p "-" alias-prefix)
    (user-error "ALIAS-PREFIX should not contain dash (-)!"))
  (let* ((fontname (downcase celeste-default-font-name))
         (fontname-normalized (string-replace " " "_" fontname))
         ;; Alias (short name) that will be recorded in `fontset-alias-alist'.
         (fontset-alias (concat "fontset-" alias-prefix "_" fontname-normalized))
         (fontsize celeste-font-size)
         (fontset-spec (format "-*-%s-regular-normal-normal-*-%d-*-*-*-p-0-%s"
                               fontname fontsize fontset-alias)))
    (create-fontset-from-fontset-spec fontset-spec)
    (celeste--fontset-setup-cjk fontset-alias)
    fontset-alias))

(defun celeste/reload-font ()
  "Reload font settings.

Call this function to apply the changes of the following options:

- `celeste-default-font-name'
- `celeste-cjk-font-name'
- `celeste-font-size'"
  (interactive)
  (let ((fontset-name (celeste--create-fontset "celeste")))
    (if-let ((frame-font-param (assoc 'font default-frame-alist)))
        (setf (cdr frame-font-param) fontset-name)
      (push `(font . ,fontset-name) default-frame-alist))
    (when (and after-init-time  ; REVIEW: is this the "best practice"?
               (selected-frame))
      (set-frame-font fontset-name))))

(celeste/reload-font)


;;; Magical multi-font settings.

(defun celeste/buffer-set-other-font (&optional font-family no-cjk-setup)
  "Setup another font for the current buffer.

If FONT-FAMILY is non-nil, use the specified font. Otherwise,
`celeste-other-font-name' is used.

Use `celeste--fontset-setup-cjk' to setup CJK charsets unless NO-CJK-SETUP is
non-nil."
  (interactive (list (completing-read (format-prompt "Font family" celeste-other-font-name)
                                      (font-family-list))))
  (let ((font-family (or font-family celeste-other-font-name)))
    (if (string= font-family celeste-default-font-name)
        (buffer-face-mode -1)
      (progn
        (setq-local buffer-face-mode-face `(:family ,font-family))
        (buffer-face-mode +1)
        (unless no-cjk-setup
          (celeste--fontset-setup-cjk))))))

(use-package nerd-icons
  :init
  (setq nerd-icons-font-family celeste-nerd-icon-font-name))



(provide 'init-font)
;;; init-font.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
