;;; init-ai.el -- LLM supports. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :init
  (celeste/prepare-package transient)
  (celeste/prepare-package compat)
  (celeste/prepare-package gptel)
  :commands (gptel
             gptel-send)
  :config
  ;; `gptel-curl-get-response'
  (require 'gptel-curl)
  ;; `gptel-system-prompt'
  (require 'gptel-transient)

  (setq gptel-display-buffer-action (list #'display-buffer-same-window))
  )


(provide 'init-ai)
;;; init-ai.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
