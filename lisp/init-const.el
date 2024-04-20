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

(defmacro celeste/add-mode-hook (mode-list function)
  "Add FUNCTION to MODE-hook for each MODE in MODE-LIST."
  (declare (indent 2))
  (let ((mode (make-symbol "mode")))
    `(dolist (,mode ,mode-list)
       (add-hook (intern (concat (symbol-name ,mode) "-hook")) ,function))))


;;; The following constant declarations are copied from Centaur Emacs.
;; Copyright (C) 2006-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

(provide 'init-const)
;;; init-const.el ends here
