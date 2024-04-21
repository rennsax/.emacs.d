;;; init-lua.el -- Lua support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

(require 'init-package)

(celeste/use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2))

(provide 'init-lua)
;;; init-lua.el ends here
