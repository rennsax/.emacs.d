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

(defcustom celeste-visual-line-mode-list
  '(message-mode
    text-mode
    debugger-mode
    compilation-mode
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
  '(special-mode eshell-mode Custom-mode)
  "List of modes that should use another font."
  :group 'celeste
  :type '(list (symbol)))

(defcustom celeste-default-font-name "MonaspiceRn Nerd Font"
  "The default font of Celeste Emacs."
  :group 'celeste
  :type 'string)

(defcustom celeste-other-font-name "MonaspiceAr Nerd Font"
  "Another font for Celeste Emacs."
  :group 'celeste
  :type 'string)

(defcustom celeste-cjk-font-name "LXGW WenKai"
  "CJK font for Celeste Emacs."
  :group 'celeste
  :type 'string)

(defcustom celeste-font-size 14
  "Font size of Celeste Emacs."
  :group 'celeste
  :type 'number)

(provide 'init-custom)
;;; init-custom.el ends here
