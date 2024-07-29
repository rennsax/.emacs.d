;;; init-lib.el -- Auxiliary functions for initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))


;;; Functions for configuring more easily.

(defun celeste--mode->hook (mode)
  (intern (concat (symbol-name mode) "-hook")))

(defun celeste--mode-hook-reset (prev-modes next-modes hook-fun)
  (dolist (mode prev-modes)
    (remove-hook (celeste--mode->hook mode) hook-fun))
  (dolist (mode next-modes)
    (add-hook (celeste--mode->hook mode) hook-fun)))

(defmacro celeste--mode-list-setter (hook-fun)
  (when (memq (car-safe hook-fun) '(lambda defun quote))
    (setq hook-fun (eval hook-fun)))
  `(lambda (sym val)
     (celeste--mode-hook-reset
      (if (boundp sym) (symbol-value sym) '())
      val
      #',hook-fun)
     (set sym val)))

(defun celeste/add-mode-hook (mode-list function)
  "Add FUNCTION to MODE-hook for each MODE in MODE-LIST."
  (declare (indent 2))
  (dolist (mode mode-list)
    (add-hook (celeste--mode->hook mode) function)))

(defun celeste/make-path (path type)
  "Make a path for cache and user data.

TYPE is one of cache and data. PATH is the relative path name."
  (let* ((var-sym (intern (format "celeste-%s-dir" type)))
         (base-dir
          (if (boundp var-sym)
              (eval var-sym)
            (error "Unrecognized type: %s!" type)))
         (file-path (concat base-dir path)))
    (if (directory-name-p file-path)
        (make-directory file-path t)
      (make-directory (file-name-directory file-path) t))
    file-path))



;;; My package management routines.


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

(make-obsolete 'celeste/use-package 'celeste/prepare-package "2024-05-13")

(defmacro celeste/autoload (function feature &rest args)
  "Define FUNCTION to autoload from FEATURE.

This is a thin wrapper of `autoload'. The file that declares
FUNCTION is found in the celeste's standard autoload function
directory `celeste-autoload-dir', with the same name of FEATURE.

ARGS are passed to `autoload' as the remaining arguments."
  (let ((file (concat celeste-autoload-dir (symbol-name feature))))
    `(autoload ,function ,file ,@args)))

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

(make-obsolete 'celeste/add-special-load-path 'celeste/prepare-package "2024-05-13")

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

(defmacro celeste/prepare-package (package &rest subpath)
  "Add PACKAGE's SUBPATHs to `load-path'.

Check if PACKAGE has been loaded. If you want to add something to
`load-path' no mater PACKAGE is loaded before, use
`celeste/prepare-package-2'.

If PACKAGE is a list, SUBPATH is ignored and all elements in the list are passed
to `celeste/prepare-package' again. Useful when a package could have multiple
dependencies.

If the directory for PACKAGE is not found (checked when the macro
is expanded), a warning is thrown."
  (cond
   ((listp package)
    (macroexp-progn
     ;; Recursively call `celeste/prepare-package'.
     (mapcar (lambda (args)
               (if (listp args)
                   `(celeste/prepare-package ,@args)
                 `(celeste/prepare-package ,args)))
             package)))
   ((symbolp package)
    (let* ((package-name (symbol-name package))
           (package-dir (concat celeste-package-dir package-name)))
      (if (file-directory-p package-dir)
          `(unless (featurep ',package)
             (eval-and-compile
               ,@(if (= (length subpath) 0)
                     `((add-to-list 'load-path ,package-dir))
                   (mapcar (lambda (p)
                             `(add-to-list 'load-path ,(file-name-concat package-dir p)))
                           subpath))))
        (warn "cannot find package: %s" package) nil)))
   (t
    (user-error "Package %s cannot be recognized" package))))

(defmacro celeste/prepare-package-2 (package &rest args)
  "Prepare PACKAGE's load path(s) or Info doc path(s) according to ARGS.

Always try to add something to corresponding paths, even PACKAGE
is loaded before.

Check if the directory for PACKAGE exists when the macro is expanded.

:info           Add the following subpath to `Info-default-directory-list'.
:load-path      Add the following subpath to `load-path'."
  (declare (indent 1))
  (cond
   ((listp package)
    (macroexp-progn
     ;; Recursively call `celeste/prepare-package'.
     (mapcar (lambda (args)
               (if (listp args)
                   `(celeste/prepare-package-2 ,@args)
                 `(celeste/prepare-package-2 ,args)))
             package)))
   ((symbolp package)
    (let* ((package-name (symbol-name package))
           (package-dir (concat celeste-package-dir package-name)))
      (if (file-directory-p package-dir)
          `(eval-and-compile
             ,@(if (= (length args) 0)
                   `((add-to-list 'load-path ,package-dir))
                 (celeste/prepare-package-2--routine package-dir args)))
        (warn "cannot find package: %s" package) nil)))
   (t
    (user-error "Package %s cannot be recognized" package))))

(defun celeste/prepare-package-2--routine (package-dir args)
  (let ((add-path t))
    (cl-loop for arg in args
             when (stringp arg)
             collect `(add-to-list ',(if add-path 'load-path 'Info-default-directory-list)
                                   ,(file-name-concat package-dir arg))
             when (keywordp arg) do
             (pcase arg
               (:info (setq add-path nil)
                      (require 'info))
               (:load-path (setq add-path t))))))


;;; Autoload routines.

;; Why is autoload still a need? Some packages, like major modes for various
;; languages, provide pre-defined entries in `auto-mode-alist'. It's tried to
;; copy them manually. In this case, autoload is still important to defer the
;; loading of these packages while keep things available.

(defun celeste/package--autoload-file-name (package)
  (expand-file-name (format "%s-autoloads.el" package)
                    (concat celeste-package-dir (symbol-name package))))

(defun celeste/package--installed-packages ()
  (ignore-errors
    (directory-files celeste-package-dir nil
                     directory-files-no-dot-files-regexp)))

(defun celeste/package--read-package-name ()
  (let* ((package-list (celeste/package--installed-packages))
         (def (thing-at-point 'symbol)))
    (when (and def (not (member def package-list)))
      (setq def nil))
    (let ((package-name (completing-read (format-prompt "Package name" def)
                                         package-list nil t nil nil def)))
      (if (string-empty-p package-name)
          (error "Empty package name!")
        package-name))))

(defun celeste/package-build-autoload (package &optional lisp-dir)
  "Generate the autoload file for PACKAGE.

By default, the directory where the package's Lisp files are
located is generated according to `celeste-package-dir'. The
optional parameter LISP-DIR gives the directory manually."
  (interactive (list (intern (celeste/package--read-package-name))))
  (let* ((pkg-dir (concat celeste-package-dir (symbol-name package)))
         (lisp-dir (or lisp-dir pkg-dir))
         (output-file (celeste/package--autoload-file-name package))

         (backup-inhibited nil)
         (version-control 'never))
    (loaddefs-generate
     lisp-dir output-file nil
     ;; The same kludge from `package-generate-autoloads'.
     (prin1-to-string
      '(add-to-list
        'load-path
        (or (and load-file-name
                 (directory-file-name
                  (file-name-directory load-file-name)))
            (car load-path)))))))

(defun celeste/package-autoload (package)
  "Load the generated autoload file of PACKAGE."
  (let ((autoload-file (celeste/package--autoload-file-name package)))
    (if (file-exists-p autoload-file)
        (with-demoted-errors "%S"
          (load autoload-file nil nil t))
      ;; If the autoload file is not found, try to build it.
      (celeste/package-build-autoload package)
      (unless (file-exists-p autoload-file)
        (error "Fail to build autoload file for %s!" package))
      (with-demoted-errors "%S"
        (load autoload-file nil nil t)))))



(provide 'init-lib)
;;; init-lib.el ends here
