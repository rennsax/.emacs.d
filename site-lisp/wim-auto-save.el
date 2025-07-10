;;; wim-auto-save.el -- What I mean auto save -*- lexical-binding: t -*-

;; Copyright (C) 2024  Bojun Ren

;; This file is not part of GNU Emacs.

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

;; What I mean auto save.

;; Emacs's auto-save is bizarre. Its behavior is different with other modern
;; editors - they just help you click the "save" button. Emacs's auto-save aims
;; to protect against loss of data when the power of your machine is suddenly
;; cut. Sometimes we need an more intuitive auto-save to prevent pressing the
;; save key for multiple times.

;; `wim-auto-save' (What-I-Mean Auto Save) provides a global minor mode
;; `wim-auto-save-mode' that does this stuff. It just helps you save buffer in
;; the behavior that you mean - use `save-buffer' (more detailed,
;; `basic-save-buffer', but they are similar).

;; `wim-auto-save' exports some customizable variables that allows to tweak the
;; auto-save behavior:

;; - `wim-auto-save-interval'
;; - `wim-auto-save-inhibit-actions': control which hooks should be disabled
;;   during auto-save.
;; - `wim-auto-save-disable-predicates': whether `wim-auto-save' should not
;;   consider the current buffer
;; - `wim-auto-save-log-level': logging level.

;; See detailed information by `M-x customize-group wim-auto-save'.

;; Thanks: this package is inspired by ChillarAnand/real-auto-save and
;; manateelazycat/auto-save.

;;; Code:

(defgroup wim-auto-save nil
  "What I mean auto save."
  :prefix "wim-auto-save-"
  :group 'editing
  :group 'convenience)


(defcustom wim-auto-save-log-level 'basic
  "Whether to pop messages in the minibuffer when auto save."
  :type '(choice :tag "Logging level"
                 (const :tag "verbose" verbose)
                 (const :tag "basic" basic)
                 (const :tag "no logging" nil)))

(defcustom wim-auto-save-disable-predicates '()
  "Predicates indicating the buffer should not be auto-saved.

Each predicate is called without arguments in the buffer to be auto-saved. If
any predicate returns non-nil value, the buffer is not auto-saved."
  :type '(repeat function))

(defcustom wim-auto-save-interval 1.0
  "Auto save interval in seconds.

Auto save is triggered when Emacs is idle for that seconds.

Caveat: you need to restart `wim-auto-save-mode' after directly setting its
value."
  :type '(choice integer float)
  :set (lambda (sym val)
         (set sym val)
         (when (bound-and-true-p wim-auto-save-mode)
           (wim-auto-save--timer-reset))))

(defcustom wim-auto-save-inhibit-actions '()
  "Disabled hooks when do auto-save routine.

Under the hood, `wim-auto-save-mode' uses `basic-save-buffer' to save each
buffer. But `basic-save-buffer' runs several hooks: `write-file-functions',
`write-contents-functions', `before-save-hook',
`after-save-hook',... (See `(elisp)Saving Buffers'.) If's unreasonable to run
these hooks so frequently.

Caveat: you should set this variable *before* `wim-auto-save-mode' is enabled."
  :type '(repeat (choice symbol
                         (cons symbol integer)))
  :set (lambda (sym val)
         (when (bound-and-true-p wim-auto-save-mode)
           (user-error "This option cannot be set when `wim-auto-save-mode' is enabled"))
         (set sym val)))

(defcustom wim-auto-save-disable-in-minibuf t
  "Whether to auto save when the cursor is at minibuffer."
  :type 'boolean)

(defvar wim-auto-save--doing nil "Whether during auto-save.")
(defvar wim-auto-save--count 0 "Global counter for auto-save.")
(defvar wim-auto-save--timer nil)

(defvar wim-auto-save--debug nil "Toggle `wim-auto-save' debug mode.")

(defun wim-auto-save--save-buffers ()
  "Try to auto save each buffer."
  (let ((n-saved-buffer 0)
        (log-level (cond
                    ((null wim-auto-save-log-level) 0)
                    ((eq wim-auto-save-log-level 'basic) 1)
                    ((eq wim-auto-save-log-level 'verbose) 2)
                    (t 1))))
    (unless (and wim-auto-save-disable-in-minibuf
                 (string-match "Minibuf" (buffer-name)))
      (save-current-buffer
        (dolist (buf (buffer-list))
          (set-buffer buf)
          (when (and
                 ;; Locally turned down?
                 (not (bound-and-true-p wim-auto-save-ignore-mode))
                 ;; Buffer is still alive?
                 (buffer-live-p buf)
                 ;; Buffer associate with a filename?
                 (buffer-file-name)
                 ;; Buffer is modifiable?
                 (buffer-modified-p)

                 ;; Any disable predicate return non-nil
                 (not (seq-some (lambda (pred) (funcall pred))
                                wim-auto-save-disable-predicates))
                 )
            (cond
             ((<= log-level 1)
              (with-temp-message ""
                (let ((inhibit-message t)
                      (inhibit-redisplay t))
                  (wim-auto-save--do-save-buffer))))
             (t
              (wim-auto-save--do-save-buffer)))
            (setq n-saved-buffer (1+ n-saved-buffer)))))
      )
    (when (> n-saved-buffer 0)
      (when (>= log-level 1)
        (message "[%d] Saved %d buffers" wim-auto-save--count n-saved-buffer))
      (setq wim-auto-save--count (1+ wim-auto-save--count)))))

(defun wim-auto-save--do-save-buffer ()
  "Do auto-save current buffer."
  (setq wim-auto-save--doing t)
  (let ((write-region-inhibit-fsync t)) ; Calling fsync hurts the performance.
    ;; `save-file' does more stuff..
    (prog1
        (basic-save-buffer)
      (setq wim-auto-save--doing nil))))

(defmacro wim-auto-save--advice-for (&optional count)
  `(defun ,(intern (format "wim-auto-save--advice-%d" (or count 0)))
       (oldfun &rest args) "Conditionally disable the function during auto-save."
       (unless ,(if (and count (not (= count 0)))
                    `(and wim-auto-save--doing
                          (not (= 0 (mod wim-auto-save--count ,count))))
                  'wim-auto-save--doing)
         ,(when wim-auto-save--debug
            `(if wim-auto-save--doing
                 (message "Run %s when count=%d" oldfun wim-auto-save--count)
               (message "Run %s when count=%d (not during auto-save)" oldfun wim-auto-save--count)))
         (apply oldfun args))))

(defun wim-auto-save--timer-kill ()
  (when wim-auto-save--timer
    (cancel-timer wim-auto-save--timer)
    (setq wim-auto-save--timer nil)))

(defun wim-auto-save--timer-reset ()
  (wim-auto-save--timer-kill)
  (setq wim-auto-save--timer
        (run-with-idle-timer wim-auto-save-interval 'repeat #'wim-auto-save--save-buffers)))

(defun wim-auto-save--inhibit-advice-add ()
  (dolist (func-or-cons wim-auto-save-inhibit-actions)
    (cond
     ((symbolp func-or-cons)
      (advice-add func-or-cons :around (wim-auto-save--advice-for)
                  '((depth . -100))))
     ((consp func-or-cons)
      (advice-add (car func-or-cons) :around
                  (eval
                   `(wim-auto-save--advice-for ,(cdr func-or-cons)))
                  `((depth . -100)))))))

(defun wim-auto-save--inhibit-advice-remove ()
  (dolist (func-or-cons wim-auto-save-inhibit-actions)
    (cond
     ((symbolp func-or-cons)
      (advice-remove func-or-cons (wim-auto-save--advice-for)))
     ((consp func-or-cons)
      (advice-remove (car func-or-cons)
                     (eval
                      `(wim-auto-save--advice-for ,(cdr func-or-cons))))))))

(defun wim-auto-save--setup ()
  (wim-auto-save--timer-reset)
  (wim-auto-save--inhibit-advice-add))

(defun wim-auto-save--teardown ()
  (wim-auto-save--timer-kill)
  (wim-auto-save--inhibit-advice-remove))


;;;###autoload
(define-minor-mode wim-auto-save-mode
  "Toggle `wim-auto-save-mode'."
  :lighter " AS"
  :init-value nil
  :global t
  :keymap nil
  (if wim-auto-save-mode
      (wim-auto-save--setup)
    (wim-auto-save--teardown)))

(define-minor-mode wim-auto-save-ignore-mode
  "Turn down `wim-auto-save-mode' locally."
  :init-value nil
  :global nil
  :keymap nil)



(provide 'wim-auto-save)
;;; wim-auto-save.el ends here
