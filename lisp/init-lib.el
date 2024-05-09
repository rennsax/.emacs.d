;;; init-lib.el -- Auxiliary functions for initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun celeste/open-init-file ()
  "Open Celeste init file."
  (interactive)
  (find-file celeste-init-file))

(defun celeste/require (package &optional path)
  "Try to require PACKAGE.

This is a thin wrapper of `require'. The file that provides
PACKAGE (as a feature) should be found at celeste's standard
package directory `celeste-package-dir'.

If PATH is nil, by default a deduced path is used, according to
the name of PACKAGE."
  (require package (or path (let ((s-p (symbol-name package)))
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

(defun celeste/add-mode-hook (mode-list function)
  "Add FUNCTION to MODE-hook for each MODE in MODE-LIST."
  (declare (indent 2))
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-hook")) function)))

(defun celeste/make-path (path type)
  "Make a path for cache and user data.

TYPE is one of cache and data. PATH is the relative path name."
  ;; TODO create directory.
  (let ((base-dir (pcase type
                    ('cache celeste-cache-dir)
                    ('data celeste-data-dir)
                    (_ (error "Unrecognized type: %s" type)))))
    (concat base-dir path)))

(defun celeste/add-special-load-path (package &optional subdir)
  "Add the Lisp directory of PACKAGE from magit to `load-path'.

Some packages (like magit, org) ship their Lisp files in the
subdirectory \"lisp/\". This function is a convenient wrapper to
dead with that situation.

Optional SUBDIR indicates that the subdirectory SUBDIR is added
to `load-path'. By default it's \"lisp\"."
  (let ((subdir (or subdir "lisp")))
    (add-to-list 'load-path (file-name-concat celeste-package-dir
                                              (symbol-name package) subdir))))

(defvar celeste/auto-require--search-dirs
  (list celeste-package-dir celeste-site-lisp-dir))

(defun celeste/auto-require (package)
  "Automatically require PACKAGE.

This function is for experimental purpose, for example, testing a
new package. If the PACKAGE can't be `require'd directly, it will
search `celeste-package-dir' and `celeste-site-lisp-dir'. If the
PACKAGE still can't be found, then raise an error."
  (unless (symbolp package) (error "PACKAGE must be a Lisp symbol!"))
  (or (ignore-errors (require package)) ; fail to require initially
      (or (let ((package-name (symbol-name package)))
            (cl-dolist (search-dir celeste/auto-require--search-dirs)
              (let ((maybe-dir (expand-file-name package-name search-dir)))
                (when (and (file-directory-p maybe-dir)
                           (file-exists-p (expand-file-name (concat package-name ".el") maybe-dir)))
                  (add-to-list 'load-path maybe-dir)
                  (cl-return (require package))))))
          (error "PACKAGE cannot be found!"))))

(provide 'init-lib)
;;; init-lib.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
