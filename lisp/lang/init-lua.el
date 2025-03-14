;;; init-lua.el -- Lua support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lua-mode
  :init
  (celeste/prepare-package lua-mode)
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2))


(provide 'init-lua)
;;; init-lua.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
