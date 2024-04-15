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
(defconst celeste-custom-file
  (concat user-emacs-directory "custom.el"))
(defconst celeste-local-dir
  (concat user-emacs-directory ".local/"))
(defconst celeste-cache-dir
  (concat celeste-local-dir "cache/"))
(defconst celeste-data-dir
  (concat celeste-local-dir "etc/"))

(defconst celeste-leader-key "SPC")

;; TODO ensure
;(eval-when-compile
;  (require 'f)
;  (mapc (lambda (dir)
;          (unless (f-dir? dir)
;            (make-directory dir 'parents)))
;        (list celeste-cache-dir
;              celeste-data-dir)))

(defmacro celeste/require (package &optional path)
  "Try to require PACKAGE.

This is a thin wrapper of `require'. The file that provides
PACKAGE (as a feature) should be found at celeste's standard
package directory `celeste-package-dir'.

If PATH is nil, by default a deduced path is used, according to
the name of PACKAGE."
  `(require ',package ,(or path (let ((s-p (symbol-name package)))
                                  (file-name-concat celeste-package-dir s-p s-p)))))

(defmacro celeste/use-package (package &rest plist)
  "Declare an Emacs package.

This is a thin wrapper of `use-package'. PACKAGE is found from
`celeste-package-dir', by providing a default value for the
`:load-path' attribute. PLIST is passed to `use-package' as the
remaining arguments."

  (declare (indent defun))
  `(use-package ,package
          ,@(unless (memq :load-path plist)
              `(:load-path ,(concat celeste-package-dir (symbol-name package))))
          ,@plist))

(defmacro celeste/autoload (function feature &rest args)
  "Define FUNCTION to autoload from FEATURE.

This is a thin wrapper of `autoload'. The file that declares
FUNCTION is found in the celeste's standard autoload function
directory `celeste-autoload-dir', with the same name of FEATURE.

ARGS are passed to `autoload' as the remaining arguments."
  (let ((file (concat celeste-autoload-dir (symbol-name feature))))
    `(autoload ,function ,file ,@args)))

;; (defmacro celeste/autoload (feature &rest functions)
;;   (declare (indent 1))
;;   (let ((func (make-symbol "func"))
;;         (file (concat celeste-autoload-dir (symbol-name feature))))
;;     `(dolist (,func (list ,@functions))
;;        (autoload ,func ,file))))



(provide 'init-const)
;;; init-const.el ends here
