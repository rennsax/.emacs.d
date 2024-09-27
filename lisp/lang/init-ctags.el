;;; init-ctags.el -- Ctags support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Citre is the frontend of universal-ctags. Universal-ctags is an elegant tool to read huge codebase.
(use-package citre
  :init (celeste/prepare-package citre)
  :commands (citre-mode))


(provide 'init-ctags)
;;; init-ctags.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
