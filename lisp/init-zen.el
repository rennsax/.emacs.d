;;; init-zen.el -- Zen mode. -*- lexical-binding: t -*-
;;; Commentary:

;; Zen mode support, leverage olivetti-mode.

;;; Code:

;; Olivetti is a brilliant package that does only one thing and does it very
;; well - centralize the buffer. Nothing more, nothing less. It does the stuff
;; by `set-window-margins'.
(use-package olivetti
  :init
  (celeste/prepare-package olivetti)

  ;; Turn on `olivetti-mode' when browsing help page.
  (add-hook 'help-mode-hook #'olivetti-mode)
  (with-eval-after-load 'helpful
    (add-hook 'helpful-mode-hook #'olivetti-mode))

  :commands olivetti-mode
  :diminish olivetti-mode
  :config
  (setq olivetti-style nil)
  ;; By default it contains `visual-line-mode', which I do not want.
  (setq olivetti-mode-on-hook nil)
  (setq-default olivetti-body-width (+ fill-column 8)))

(defvar celeste/zen-mode-disabled-mode-list '()
  "Modes that need be deleted when zen-mode is on.")

(defun celeste/zen-mode-add-disabled-mode (mode &optional after)
  (if after
      (with-eval-after-load after
        (add-to-list 'celeste/zen-mode-disabled-mode-list mode))
    (add-to-list 'celeste/zen-mode-disabled-mode-list mode)))

(celeste/zen-mode-add-disabled-mode 'display-line-numbers-mode 'display-line-numbers)
(celeste/zen-mode-add-disabled-mode 'diff-hl-mode 'diff-hl)
(celeste/zen-mode-add-disabled-mode 'display-fill-column-indicator-mode 'display-fill-column-indicator)

;; Not recommended, since `tab-bar-mode' is a global minor mode.
; (celeste/zen-mode-add-disabled-mode 'tab-bar-mode 'tab-bar)

(define-minor-mode celeste/zen-mode
  "Toggle buffer-local Zen mode."
  :init-value nil
  :lighter "  "
  :global nil
  (if celeste/zen-mode
      (progn
        (olivetti-mode +1)
        (set-window-fringes (selected-window) 0 0)
        (dolist (mode-to-disable celeste/zen-mode-disabled-mode-list)
          (let ((old-var-sym (intern (concat "zen--old-" (symbol-name mode-to-disable)))))
            (set (make-local-variable old-var-sym) (symbol-value mode-to-disable))
            (funcall mode-to-disable -1))))
    (olivetti-mode -1)
    (set-window-fringes (selected-window) nil)
    (dolist (old-mode celeste/zen-mode-disabled-mode-list)
      (let ((old-var-sym (intern (concat "zen--old-" (symbol-name old-mode)))))
        (when (symbol-value old-var-sym)
          (funcall old-mode +1))
        (kill-local-variable old-var-sym)))))

(keymap-global-set "C-c z z" #'celeste/zen-mode)

(provide 'init-zen)
;;; init-zen.el ends here


;; Local Variables:
;; no-byte-compile: t
;; End:
