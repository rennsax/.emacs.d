;;; init-ai.el -- LLM supports. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; gptel
(use-package gptel
  :init
  (celeste/prepare-package (transient markdown-mode))
  (celeste/package-autoload 'gptel)
  (with-eval-after-load 'embark
    (defvar-keymap embark-gptel-map
      :doc "Keymap for gptel."
      :parent nil
      "a" #'gptel-add
      "m" #'gptel-menu
      "s" #'gptel-send)
    (fset 'embark-gptel-map embark-gptel-map)
    (keymap-set embark-region-map "g" #'embark-gptel-map))
  :bind (:map gptel-mode-map
              ("C-c C-n" . gptel-end-of-response)
              ("C-c C-p" . gptel-beginning-of-response)
              ("C-c C-k" . gptel-abort))
  :config
  (setq gptel-display-buffer-action nil)
  (setq gptel-default-mode 'org-mode)
  (add-hook 'gptel-mode-hook
            (defun +gptel-mode-override-settings ()
              (when (fboundp 'whitespace-display-mode)
                (whitespace-display-mode -1))
              (auto-fill-mode -1)
              (visual-line-mode +1)))

  ;; Customize prompt/response prefixes. These prefixes will be trimmed to form
  ;; the final request body, check `gptel--parse-buffer'.
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "**Prompt**: ")
          (org-mode . "*Prompt*: ")
          (text-mode . "### ")))
  (setq gptel-response-prefix-alist
        '((markdown-mode . "**Response**:\n")
          (org-mode . "*Response*:\n")
          (text-mode . "")))

  ;; This is necessary for using `gptel-menu' in Vterm buffer.
  (with-eval-after-load 'vterm
    (keymap-unset vterm-copy-mode-map "<return>" t))

  )



(provide 'init-ai)
;;; init-ai.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
