;;; init-const.el -- Constants for Celeste Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst celeste-init-file
  (concat user-emacs-directory "init.el"))
(defconst celeste-package-dir
  (concat user-emacs-directory "packages/"))
(defconst celeste-site-lisp-dir
  (concat user-emacs-directory "site-lisp/"))
(defconst celeste-lisp-dir
  (concat user-emacs-directory "lisp/"))
(defconst celeste-autoload-dir
  (concat celeste-lisp-dir "autoload/"))

(defconst celeste-leader-key "SPC")

;; Celeste Emacs directs temporary files created by Emacs to a unified path, so
;; the root path `user-emacs-directory' won't be too dirty.
(defconst celeste-local-dir
  (concat user-emacs-directory ".local/"))
(defconst celeste-cache-dir
  (concat celeste-local-dir "cache/"))
(defconst celeste-data-dir
  (concat celeste-local-dir "etc/"))

;; Operating system.
(defconst sys/mac (eq system-type 'darwin))
(defconst sys/linux (eq system-type 'gnu/linux))
(defconst sys/win (memq system-type '(ms-dos windows-nt)))

(provide 'init-const)
;;; init-const.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
