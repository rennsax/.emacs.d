;;; init-editor.el -- Editor feature enhancement. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; `anzu.el' provides a minor mode which displays 'current match/total
(celeste/use-package anzu
  :hook (after-init . global-anzu-mode))

;; Rather convenient package that can serve as a complete alternative of Vim's
;; textobject!
(celeste/use-package expand-region
  :commands er/expand-region
  :bind (("C-=" . er/expand-region)))

;; Celeste Emacs has provided presets for several modal editing packages.
(pcase celeste-modal-editing
  ('meow (require 'init-meow))
  ('evil
   (require 'init-evil)
   (require 'init-evil-keybinding)))

(provide 'init-editor)
;;; init-editor.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
