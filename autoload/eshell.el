;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +eshell-kill-whole-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (let ((end (progn (end-of-visual-line) (point))))
        (kill-region eshell-last-output-end end)))

;;;###autoload
(defun +eshell-input-bol ()
  (interactive)
  (set-window-point nil eshell-last-output-end))
