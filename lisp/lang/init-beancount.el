;;; init-beancount.el -- Beancount -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package beancount
  :init (celeste/prepare-package beancount)
  :mode (("\\.beancount\\'" . beancount-mode)
         ("\\.bean\\'" . beancount-mode))
  :config
  (setq beancount-transaction-indent 4)
  (setq beancount-use-ido nil)
  (setq beancount-number-alignment-column 0))


(provide 'init-beancount)
;;; init-beancount.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
