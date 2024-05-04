;;; init-corfu.el -- Corfu completion frontend. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Lightweight completion engine, powered by capf.
;; Now only used by eshell and emacs-lisp-mode.
(celeste/use-package corfu
  :commands corfu-quit corfu-mode
  :config
  (setq corfu-auto t
        corfu-auto-prefix 3
        ;; Do not do anything (such as the default "insert") on exact match. So
        ;; a manual TAB is always needed for completion.
        corfu-on-exact-match nil
        )
  ;; Disable RET -> `corfu-insert'. I prefer primitive RET - just `newline'.
  (mapc (lambda (key) (keymap-unset corfu-map key))
        '("RET" "<remap> <next-line>" "<remap> <previous-line>"
          "<remap> <beginning-of-buffer>" "C-a"))

  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  ;; Toggle corfu in all buffers
  :hook (;; In `eshell-mode', do not automatically toggle corfu prompt.
         (eshell-mode . (lambda ()
                          ;; Must before `corfu-mode' is turned on.
                          (setq-local corfu-auto nil)
                          (corfu-mode)))
         (emacs-lisp-mode . corfu-mode)))

;; Lightweight templates.
(celeste/use-package tempel
  :preface
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions))
    (tempel-abbrev-mode))
  :hook ((conf-mode prog-mode text-mode) . tempel-setup-capf)
  :commands (tempel-expand tempel-abbrev-mode tempel-complete)
  :config
  (setq tempel-trigger-prefix "t|")
  )

(provide 'init-corfu)
;;; init-corfu.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
