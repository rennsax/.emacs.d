;;; init-const.el -- Constants for Celeste Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst celeste-init-file
  (expand-file-name
   (concat user-emacs-directory "init.el")))
(defconst celeste-package-dir
  (expand-file-name
   (concat user-emacs-directory "packages/")))
(defconst celeste-site-lisp-dir
  (expand-file-name
   (concat user-emacs-directory "site-lisp/")))
(defconst celeste-lisp-dir
  (expand-file-name
   (concat user-emacs-directory "lisp/")))
(defconst celeste-autoload-dir
  (expand-file-name
   (concat user-emacs-directory "autoload/")))

;; Celeste Emacs directs temporary files created by Emacs to a unified path, so
;; the root path `user-emacs-directory' won't be too dirty.
(defconst celeste-local-dir
  (expand-file-name
   (concat user-emacs-directory ".local/")))
(defconst celeste-cache-dir
  (expand-file-name
   (concat celeste-local-dir "cache/")))
(defconst celeste-data-dir
  (expand-file-name
   (concat celeste-local-dir "etc/")))
(defconst celeste-state-dir
  (expand-file-name
   (concat celeste-local-dir "state/")))

;; Operating system.
(defconst sys/mac (eq system-type 'darwin))
(defconst sys/linux (eq system-type 'gnu/linux))
(defconst sys/win (memq system-type '(ms-dos windows-nt)))

(provide 'init-const)
;;; init-const.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
