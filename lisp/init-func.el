;;; init-func.el -- Useful interactive functions. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

(defun celeste/open-init-file ()
  "Open celeste init file."
  (interactive)
  (find-file celeste-init-file))

(defun celeste/reload-init-file ()
  "Reload celeste init file."
  (interactive)
  (load-file celeste-init-file))

;; Show init time after configurations are fully loaded.
(add-hook 'emacs-startup-hook
          (lambda () (message
                      (format "Init time: %s."
                              (emacs-init-time)))))


(provide 'init-func)
;;; init-func.el ends here
