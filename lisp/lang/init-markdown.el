;;; init-markdown.el -- Markdown support. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; For further usage, read https://github.com/jrblevin/markdown-mode#usage.
(celeste/use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ;; Github Flavoured Markdown files
         ("README\\.md\\'" . gfm-mode))
  :config
  ;; Basic settings.
  (setq markdown-italic-underscore t ; Prefer underscores over asterisks when
                                     ; `markdown-insert-italic'.
        ;; `markdown-insert-header-dwim' insert "## %s" rather than "## %s ##"
        markdown-asymmetric-header t
        ;; `gfm-mode' checkboxs are truned into buttons.
        markdown-make-gfm-checkboxes-buttons t
        ;; Fontify code in code blocks using the native major mode.
        markdown-fontify-code-blocks-natively t)

  ;; Dependency for editing code blocks in markdown-mode.
  (celeste/use-package edit-indirect)
  )

(celeste/use-package markdown-toc
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))

(provide 'init-markdown)
;;; init-markdown.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
