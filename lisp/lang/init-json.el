;;; init-json.el -- Json support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Getting the path to a JSON value at point. Required by json-mode.
(use-package json-snatcher
  :init
  (celeste/prepare-package json-snatcher))
;; json-mode and jsonc-mode
(celeste/package-autoload 'json-mode)

(provide 'init-json)
;;; init-json.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
