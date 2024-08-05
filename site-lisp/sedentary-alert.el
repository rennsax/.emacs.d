;;; sedentary-alert.el -- Alert for sedentary -*- lexical-binding: t -*-

;; Copyright (C) 2024  Bojun Ren

;; Author: Bojun Ren <bj.ren.coding@outlook.com>
;; Maintainer: Bojun Ren <bj.ren.coding@outlook.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (alert "1.2"))
;; Keywords: notify, notifications, health

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

;;; Commentary:

;; Dear programmer, pay attention to your personal health!

;;; Code:

(require 'alert)

(defgroup sedentary-alert nil
  "Sedentary alert."
  :group 'notify
  :group 'notifications
  :group 'health)

(defcustom sedentary-alert-interval (* 50 60)
  "Seconds between each stand up. The default value is 3000s, 50min."
  :group 'sedentary-alert
  :type 'integer)

;;;###autoload
(defun sedentary-alert ()
  "Show alert for long-time sedentary."
  (interactive)
  (alert "Stand up!" :title "Sedentary Alert"))

(defvar sedentary-alert--timer nil)

(defun sedentary-alert--teardown ()
  "Cancel the timer for sedentary alert."
  (when sedentary-alert--timer
    (cancel-timer sedentary-alert--timer)
    (setq sedentary-alert--timer nil)))

(defun sedentary-alert-reset ()
  "Reset the timer for alerting sedentary."
  (interactive)
  (sedentary-alert--teardown)
  (setq sedentary-alert--timer
        (run-at-time sedentary-alert-interval sedentary-alert-interval #'sedentary-alert)))

;;;###autoload
(define-minor-mode sedentary-alert-mode
  "Toggle `sedentary-alert-mode'."
  :init-value nil
  :global t
  :keymap nil
  (if sedentary-alert-mode
      (sedentary-alert-reset)
    (sedentary-alert--teardown)))

(provide 'sedentary-alert)
;;; sedentary-alert.el ends here
