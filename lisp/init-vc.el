;;; init-vc.el -- Version control tools. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const))

;; TODO: `magit-submodule-add-1' git submodule absortgitdirs
(celeste/use-package magit
  :init
  ;;; Add deps
  ;; compat
  ;; dash
  ;; There is also a builtin `transient' package.
  ;; However we use the submodule one.
  (celeste/use-package transient
    :load-path "packages/transient/lisp")
  (celeste/use-package with-editor
    :load-path "packages/with-editor/lisp")
  :load-path "packages/magit/lisp"
  :demand t)

;; diff-hl: better git-diff integration
;; TODO: find-dired
(add-to-list 'load-path (concat celeste-package-dir "diff-hl"))
(use-package diff-hl
  ;; `find-file-hook': after a buffer is loaded from a file
  :hook ((find-file . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))
(use-package diff-hl-flydiff
  :hook (diff-hl-mode . diff-hl-flydiff-mode))
(use-package diff-hl-show-hunk
  :commands diff-hl-show-hunk)
(use-package diff-hl-dired
  :hook (dired-mode . diff-hl-dired-mode))

(provide 'init-vc)
;;; init-vc.el ends here
