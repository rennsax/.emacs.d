;;; init-custom.el -- Customizable declarations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Bootstrap custom file.
;; NOTE: Should be as early as possible
(setq custom-file (locate-user-emacs-file "custom.el"))
;; `load' is more primitive than `load-file'.
(load custom-file t t t)

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

(provide 'init-custom)
;;; init-custom.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
