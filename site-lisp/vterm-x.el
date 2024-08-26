;;; vterm-x.el -- Vterm extensions and tweaks -*- lexical-binding: t -*-

;; Author: Bojun Ren <bj.ren.coding@outlook.com>
;; Package-Requires: ((emacs "29.3") (vterm "0.02"))
;; Version: 0.01

;;; Commentary:

;; This package provides several useful and meaningful tweaks and extensions for
;; the vterm package. It's started with my personal configurations. As they
;; grow up, I then decide to maintain them separately.

;; To use these package, make sure it can be found in your `load-path' and load
;; it somewhere:

;; (require 'vterm-x)

;;; Code:

(require 'tramp)
(require 'vterm)

(defgroup vterm-x nil
  "Vterm extensions and tweaks."
  :group 'vterm)


;;; Fundamental hook for implementing many features in this package.

(defcustom vterm-set-directory-hook nil
  "Hook run after `vterm--set-directory'."
  :type 'hook
  :group 'vterm-x)

(defcustom vterm-shell-integration-indicator
  '((:propertize " " face ((t :inherit success))) . (:propertize " " face ((t :inherit error))))
  "Symbols that indicate whether shell integration is correctly installed."
  :type '(cons sexp sexp)
  :group 'vterm-x)

(put 'vterm-shell-integration-indicator 'risky-local-variable t)

(define-advice vterm--set-directory (:after (&rest _) run-hooks -100)
  (run-hooks 'vterm-set-directory-hook))


;;; Change the vterm buffer name automatically, according to `default-directory'.

(defun vterm--generate-buffer-name (path)
  "Generate a new name for vterm buffer, according to PATH."
  (format "*vterm<%s%s>*"
          (if (file-remote-p path) "@" "")
          (file-name-base
           (directory-file-name path))))

(defun vterm--sync-buffer-name ()
  "Set the buffer name according to `default-directory'."
  (let ((newbufname (vterm--generate-buffer-name default-directory)))
    (unless (string-equal newbufname (buffer-name))
      (rename-buffer (generate-new-buffer-name newbufname)))))

(add-hook 'vterm-set-directory-hook #'vterm--sync-buffer-name)


;;; A more intuitive implementation of `vterm'.

;;;###autoload
(defun vterm-at (file &optional arg)
  "Open a vterm buffer that is closest to FILE.

If FILE is a directory, open vterm at this directory. If FILE is a regular file,
open vterm at its parent directory.

If ARG is non-nil, force to open a new vterm buffer."
  (interactive "fVterm directory: \nP")
  (unless (file-exists-p file)
    (error "FILE %s does not exist!" file))
  (unless (file-directory-p file)
    (setq file (file-name-directory file)))
  ;; Convert directory name to absolute, and remove the tailing slash.
  (setq file (directory-file-name (expand-file-name file)))
  (let* ((default-directory file)
         (vterm-buf-name
          (format "*vterm<%s>*" (file-name-base file)))
         (buf (get-buffer vterm-buf-name)))
    (if (and (not arg)
             buf
             (buffer-live-p buf)
             (file-equal-p
              (with-current-buffer buf default-directory)
              default-directory))
        (switch-to-buffer buf)
      ;; If a string is given, `vterm' will always open a new terminal.
      (vterm vterm-buf-name))))


;;; Indicate whether the shell integration is installed.

;; Limits: it's hard to tell whether shell integration is installed in a
;; subshell.

(defvar-local vterm--shell-integrated nil)
(put 'vterm--shell-integrated 'risky-local-variable t)

(defun vterm--detect-shell-integration-h ()
  "Setup modeline that can detect shell integration."
  (setq-local mode-name `(,mode-name
                          " "
                          (:eval (if vterm--shell-integrated
                                     (car vterm-shell-integration-indicator)
                                   (cdr vterm-shell-integration-indicator))))))

(defun vterm--ack-shell-integration-h ()
  (setq vterm--shell-integrated t))

(add-hook 'vterm-mode-hook #'vterm--detect-shell-integration-h)
(add-hook 'vterm-set-directory-hook #'vterm--ack-shell-integration-h)


;;; Tweaks for vterm over remote connections.

(define-advice vterm--get-directory (:around (oldfun &rest args) handle-non-user)
  "Allow `vterm--get-directory' handle paths that does not specify username.

It can be useful when the user set an abbreviation host name in its ssh config."
  (let ((path (car args)))
    (if (string-match-p "@" path)
        (apply oldfun args)
      (when (string-match
             (rx bos (group (* nonl)) ?: (group (* nonl)) eos) path)
        (file-name-as-directory (concat "/-:" path))))))

;; Make tmux work at the remote shell. https://github.com/akermu/emacs-libvterm/issues/569.
(define-advice vterm--get-shell (:filter-return (shell) quote-shell)
  "Quote the shell name if it's a remote shell."
  (replace-regexp-in-string
   (rx (group (? "~/" "./" "/") ;; Optional leading dot or tilde
              (* (+ (not space) ?/)))
       (group
        (| "sh" "ksh" "csh" "tcsh" "bash" "zsh" "fish" "nu" "xonsh") eow))
   "\\1'\\2'"
   shell))

(defcustom vterm-x-environment nil
  "Environment that will be prepended to `process-environment' when creating vterm.

Format: same as `process-environment', e.g. \\='(\"env1=v1\" \"env2=v2\")."
  :type '(repeat string)
  :group 'vterm-x)

(put 'vterm-x-environment 'risky-local-variable t)

(make-obsolete-variable
 'vterm-environment
 'vterm-x-environment "0.01"
 "Vterm does not support connection-local `vterm-environment'. Vterm-x uses
another variable to achieve this.")

(define-advice vterm-mode (:around (oldfun &rest args) set-env)
  "Set environment variables correctly for the shell sub-process.

This advice helps `vterm-mode' to:

1. Set COLORTERM.
2. Set PAGER.
3. Respect connection-local `vterm-x-environment'."
  (let ((process-environment
         (append
          ;; Respect the connection-local one.
          (with-connection-local-variables vterm-x-environment)
          ;; It's the terminal emulator's duty to set COLORTERM env. Just like iTerm2 does:
          ;; https://gitlab.com/gnachman/iterm2/-/commit/978e1ab1b2ac7847d96dbabec808dfe767d45184.
          ;; See also neovim/libvterm:vterm.c, ls(1).
          '("COLORTERM=truecolor")
          process-environment))
        ;; `tramp-remote-process-environment' is used to setup remote envs by
        ;; `tramp-open-connection-setup-interactive-shell'.
        (tramp-remote-process-environment
         (seq-remove
          ;; Do not set "PAGER=cat".
          (lambda (env) (string-match-p "\\`PAGER" env))
          tramp-remote-process-environment)))
    (apply oldfun args)))


(provide 'vterm-x)
;;; vterm-x.el ends here
