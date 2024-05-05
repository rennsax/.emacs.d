;;; init-lib.el -- Auxiliary functions for initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(defun +getenv-shell (variable &optional frame always)
  "Get the environment variable VARIABLE from shell.

VARIABLE and FRAME is passed to `getenv'.

If ALWAYS is non-nil, always try to copy env from shell.
Otherwise, if `getenv' returns non-nil, the result is returned
immediately."
  (if always
      (cdar (exec-path-from-shell-copy-env variable))
    (or (getenv variable frame)
        (cdar (exec-path-from-shell-copy-env variable)))))

(provide 'init-lib)
;;; init-lib.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
