;;; init-package.el -- Package initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable `use-package'.
(setq use-package-always-ensure nil ; do not install packages w/ package.el!
      use-package-always-defer t ; to accelerate startup
      use-package-expand-minimally t)

;; Common Lisp extensions for Emacs, builtin
(celeste/require 'dash)
(celeste/require 's)
(celeste/require 'f)
(celeste/require 'annalist)
(celeste/require 'compat) ; COMPATibility Library for Emacs Lisp
(celeste/require 'shrink-path)

;; REVIEW: `doom-modeline' does not show any minor mode factually. So do I still
;; need diminish.el? IDK, so I specify it as a lazy-loaded dependency.
(celeste/use-package diminish
  :commands diminish)



;;; My package management routines.

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
        (with-demoted-errors "Cannot load autoload file of package %s: %s"
          package (load autoload-file nil nil t))
      (user-error "Cannot find the autoload file of package %s.\nHave you run `celeste/build-autoload'?" package))))



(provide 'init-package)
;;; init-package.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
