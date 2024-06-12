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
    debugger-mode
    compilation-mode
    eshell-mode
    magit-process-mode)
  "List of modes when `visual-line-mode' should be enabled."
  :group 'celeste
  :type '(repeat symbol))

(defcustom celeste-auto-fill-mode-list
  '(org-mode)
  "List of modes when `auto-fill-mode' should be enabled."
  :group 'celeste
  :type '(repeat symbol))

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

(defcustom celeste-python-command "python3"
  "Python3 command to use for lsp-bridge and EAF."
  :group 'celeste
  :type 'string
  :set (lambda (sym val)
         ;; `setq' is a special form - if a symbol is passed as the first
         ;; argument, it won't work correctly.
         (set sym val)
         (setq lsp-bridge-python-command val)))

(provide 'init-custom)
;;; init-custom.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
