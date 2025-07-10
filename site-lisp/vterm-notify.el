;;; vterm-notify.el -- Vterm notification -*- lexical-binding: t -*-

;; Author: Bojun Ren <me.rennsax@gmail.com>
;; Package-Requires: ((emacs "29.3") (vterm "0.02") (alert "1.0") (vterm-x "0.01"))

;;; Commentary:

;; When a new prompt pops up in vterm buffer, send a notification. This feature
;; requires the shell integration to be properly installed.

;; Enable `vterm-notify-mode', and when the next prompt shows up, a notification
;; is sent, utilizing the `alert' library. After notificaition,
;; `vterm-notify-mode' is automatically disabled.

;; It's highly recommended to bind a key to `vterm-notify-mode', since if the
;; window size change, vterm will immediately send notification (see the
;; explanation in the documentation of `vterm-notify-mode'). Example:

;; (keymap-set vterm-mode-map "M-s-a" #'vterm-notify-mode)


;; TODO: prohibit `vterm--set-size'?

;;; Code:

(require 'alert)
(require 'vterm)
(require 'vterm-x)                      ; `vterm-set-directory-hook'

(defcustom vterm-notify-message "New prompt!"
  "Message to be notified by `vterm-notify'."
  :group 'vterm-notify
  :type 'string)

(defun vterm--do-notify (&optional keep)
  (alert vterm-notify-message)
  (unless keep
    (vterm-notify-mode -1)))

;;;###autoload
(define-minor-mode vterm-notify-mode
  "If on, notify on next prompt in vterm.

Shell integration should be installed correctly.

NOTE: When `vterm-notify-mode' is enabled, notification is sent when the shell
prints a control string beginning with \"51;A\". Vterm shell integration makes
your shell do that each time it draws a prompt. Redrawing the vterm buffer also
triggers the notification because the shell will print new prompts. As a result,
when the size of windows change, it will immediately notify."
  :group 'vterm
  :lighter " VTermNotify" ;; NOTE: mono-height
  :keymap nil
  :global nil
  (unless (or (eq major-mode 'vterm-mode)
              (derived-mode-p 'vterm-mode))
    (user-error "You cannot enable `vterm-notify-mode' outside vterm buffers!"))
  (unless (bound-and-true-p vterm--shell-integrated)
    (user-error "Shell integration is not installed correctly!"))
  (if vterm-notify-mode
      (add-hook 'vterm-set-directory-hook #'vterm--do-notify nil t)
    (remove-hook 'vterm-set-directory-hook #'vterm--do-notify t)))

(provide 'vterm-notify)
;;; vterm-notify.el ends here
