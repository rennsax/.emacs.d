;;; init-custom.el -- Customizable declarations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defgroup celeste nil
  "Celeste Emacs customization."
  :group 'emacs)

(defcustom celeste-org-dir
  "~/org/"
  "Directory with Org files. Also used in `org-agenda-files'."
  :group 'celeste
  :type 'directory)

(defcustom celeste-default-theme
  'doom-cobalt2
  "The default theme name."
  :group 'celeste
  :type 'symbol)

(defcustom celeste-visual-line-mode-list
  '(message-mode
    text-mode
    debugger-mode
    compilation-mode
    eshell-mode
    magit-process-mode)
  "List of modes when `visual-line-mode' should be enabled."
  :group 'celeste
  :type '(list (symbol)))

(defcustom celeste-auto-fill-mode-list
  '(org-mode)
  "List of modes when `auto-fill-mode' should be enabled."
  :group 'celeste
  :type '(list (symbol)))

(defcustom celeste-other-font-mode-list
  '(special-mode
    eshell-mode
    Custom-mode
    dired-mode
    git-commit-mode
    org-mode
    markdown-mode
    org-agenda-mode
    vterm-mode)
  "List of modes that should use another font."
  :group 'celeste
  :type '(list (symbol)))

(defcustom celeste-cjk-font-mode-list
  nil
  "List of modes that should use CJK font."
  :group 'celeste
  :type '(list (symbol)))

(defcustom celeste-default-font-name "MonaspiceRn Nerd Font"
  "The default font of Celeste Emacs. Pick whatever you like."
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

(defcustom celeste-modal-editing nil
  "Which kind of modal editing is used. If nil, use vanilla Emacs.

One of meow, evil and nil."
  :group 'celeste
  :type 'symbol)

(defcustom celeste-inherit-shell-env-list '()
  "Environment variables that should be inherited from the default shell.

Envs are obtained by `exec-path-from-shell'."
  :group 'celeste
  :type 'symbol)

(provide 'init-custom)
;;; init-custom.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
