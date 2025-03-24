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
  (use-package transient
    :config
    ;; FIXME: find out how to fix this problem in ace-window. Transient 3b28a2f
    ;; change the default value of `transient-hide-during-minibuffer-read',
    ;; which then has been obsoleted by this option.
    (setq transient-show-during-minibuffer-read t))
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

  (setq gptel--num-messages-to-send 0)
  (setq gptel-expert-commands t)

  ;; This is necessary for using `gptel-menu' in Vterm buffer.
  (with-eval-after-load 'vterm
    (keymap-unset vterm-copy-mode-map "<return>" t))

  )

(use-package gptel-rewrite
  :autoload gptel-rewrite)



(provide 'init-ai)
;;; init-ai.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
