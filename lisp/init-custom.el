;;; init-custom.el -- Customizable declarations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const))

(setq custom-file celeste-custom-file)
;; `load-file' vs `load': the previous one just execute the Lisp code in the
;; given file. The latter one does more things: try to add suffix, search
;; `load-path', ...
(load-file custom-file)

(defgroup celeste nil
  "Celeste Emacs customization."
  :group 'emacs)

(defcustom celeste-org-dir
  "~/org/"
  "Directory with Org files. Also used in `org-agenda-files'."
  :group 'celeste
  :type 'directory)


(provide 'init-custom)
;;; init-custom.el ends here
