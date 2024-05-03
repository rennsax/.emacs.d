;;; init-json.el -- Json support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO lazy load

;; Getting the path to a JSON value at point. Required by json-mode.
(celeste/require 'json-snatcher)
;; json-mode and jsonc-mode
(celeste/require 'json-mode)

(provide 'init-json)
;;; init-json.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
