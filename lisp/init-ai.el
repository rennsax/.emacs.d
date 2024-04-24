;;; init-ai.el -- LLM supports. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-package)
  (require 'init-const))

(celeste/use-package gptel
  :commands (gptel
             gptel-send)
  :config
  ;; `gptel-curl-get-response'
  (require 'gptel-curl)
  ;; `gptel-system-prompt'
  (require 'gptel-transient))


(provide 'init-ai)
;;; init-ai.el ends here
