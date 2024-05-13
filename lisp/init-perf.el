;;; init-perf.el -- Performance related. -*- lexical-binding: t -*-
;;; Commentary:

;; Focus on improving Emacs performance, like tweaking GC, JIT, etc.

;;; Code:


;; Garbage collection magic hack.
(celeste/prepare-package gcmh)
(use-package gcmh
  :diminish
  :hook (after-init . gcmh-mode)
  :config
  ;; The default is 128MB, which is too big, making each GC stuck too long.
  ;; 128MB is a reasonable value.
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))

(use-package jit-lock
  :init
  ;; If 0, then fontification is only deferred while there is input pending.
  (setq jit-lock-defer-time 0))


(provide 'init-perf)
;;; init-perf.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
