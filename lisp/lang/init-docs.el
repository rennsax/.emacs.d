;;; init-docs.el -- Lookup development docs in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Devdoc
(use-package devdocs
  :init
  (celeste/prepare-package devdocs)
  (defalias 'dev-docs-lookup 'devdocs-lookup)
  :commands devdocs-lookup devdocs-install
  :config
  (setq devdocs-data-dir (celeste/make-path "devdocs" 'data)))


(provide 'init-docs)
;;; init-docs.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
