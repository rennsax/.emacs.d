;;; connection-local-set.el -- Set connection local env in a breeze -*- lexical-binding: t -*-

;; Copyright (C) 2024  Bojun Ren

;; This file is not part of GNU Emacs. It's part of Celeste Emacs, my personal
;; Emacs configuration.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;; Author: Bojun Ren <me.rennsax@gmail.com>
;; Maintainer: Bojun Ren <me.rennsax@gmail.com>

;;; Commentary:

;; This package provides a new macro, `celeste/connection-local-set', which
;; makes it easier for setting connection local environment variables per host.

;; `celeste/connection-local-set' is basically a wrapper for
;; `connection-local-set-profiles', but exports useful interfaces for most
;; common use cases.

;;; Code:
(require 'tramp)

(cl-defmacro celeste/connection-local-set
    (host
     &key
     (application 'tramp)
     (protocol (if (eq application 'tramp) tramp-default-method "sshx"))
     ((:shell -shell-file-name))
     (vterm-env)
     (remove t))
  "Set connection local variables for HOST.

Side effects: modifying `connection-local-criteria-alist'.

HOST is passed to the `:machine' property of the criteria argument of
`connection-local-set-profiles'. It's typically the hostname of the remote
server, or the \"Host\" field in your OpenSSH client configuration file. If the
previous one is specified, then you probably want `:vterm-env t', or for the
latter one it's `:vterm-env full'.

CAVEAT: should be used after Tramp is loaded because it also modifies
`connection-local-criteria-alist'!

:application     Optional, default to `tramp'.
:protocol        Optional, if `:application' is tramp, then default to
                 `tramp-default-method', else default to \"sshx\".
:shell           Required.
:vterm-env       Optional. How `vterm-x-environment' should be set. If t, then
                 set \"VTERM_HOST=host\". If `full', then set
                 \"VTERM_FULL_HOST=host\". If a list of strings, then he list is
                 set as `vterm-x-environment.'"
  (declare (indent defun))
  (unless -shell-file-name (error "Key `:shell' must be specified!"))

  (let ((shell-profile (intern (concat "--remote-shell-"
                                       (substring
                                        (md5 -shell-file-name) 0 6))))
        exps vterm-env-profile)
    (when remove
      (push
       `(setq connection-local-criteria-alist
              (seq-remove (lambda (criteria)
                            (let ((machine (cadr (memq :machine (car criteria)))))
                              (string-equal machine ,host)))
                          connection-local-criteria-alist))
       exps))
    (push
     `(connection-local-set-profile-variables
       ',shell-profile
       '((shell-file-name . ,-shell-file-name)))
     exps)
    (when vterm-env
      (when (eq vterm-env t)
        (setq vterm-env (list (concat "VTERM_HOST=" host))))
      (when (or (eq vterm-env 'full)
                (equal vterm-env '(quote full)))
        (setq vterm-env (list (concat "VTERM_FULL_HOST=" host))))
      (unless (and (listp vterm-env)
                   (seq-every-p #'stringp vterm-env))
        (error "Key `:vterm-env' accepts `t', `full', or the same format of `vterm-x-environment'!"))
      (setq vterm-env-profile (intern (concat "--remote-vterm-env-"
                                              (substring
                                               (md5 (format "%s" vterm-env)) 0 6))))
      (push
       `(connection-local-set-profile-variables
         ',vterm-env-profile
         '((vterm-x-environment . ,vterm-env)))
       exps))
    (unless (listp protocol) (setq protocol (list protocol)))
    (setq exps (append exps
                       (mapcar (lambda (p)
                                 (append
                                  `(connection-local-set-profiles
                                    '(:application ,application :protocol ,p :machine ,host)
                                    ',shell-profile)
                                  (if vterm-env-profile `(',vterm-env-profile) nil)))
                               protocol)))
    (macroexp-progn exps)))


(provide 'connection-local-set)
;;; connection-local-set.el ends here
