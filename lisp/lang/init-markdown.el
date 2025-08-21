;;; init-markdown.el -- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; For further usage, read https://github.com/jrblevin/markdown-mode#usage.
(use-package markdown-mode
  :init
  (celeste/prepare-package markdown-mode)

  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  ;; Customized manually in UI.
  ; :custom
  ; ((markdown-header-scaling t)          ; Use variable-height faces for headers.
  ;  (markdown-header-scaling-values '(1.6 1.4 1.2 1.1 1.0 1.0)))
  :config
  ;; Basic settings.
  (setq markdown-italic-underscore t ; Prefer underscores over asterisks when
                                     ; `markdown-insert-italic'.
        ;; `markdown-insert-header-dwim' insert "## %s" rather than "## %s ##"
        markdown-asymmetric-header t
        ;; `gfm-mode' checkboxs are truned into buttons.
        markdown-make-gfm-checkboxes-buttons t
        ;; Fontify code in code blocks using the native major mode.
        markdown-fontify-code-blocks-natively t

        ;; Put # in the left margin
        ;; BUG: conflict with `olivetti-mode'
        ; markdown-marginalize-headers t
        ; markdown-marginalize-headers-margin-width 4

        markdown-url-compose-char 8230  ; dots
        )

  (setq-default markdown-hide-urls t)
  (setq-default markdown-enable-math t)

  (setq markdown-gfm-additional-languages '("console"))

  (celeste/setup-lang markdown
    :modes (markdown-mode)
    :eglot-server ("marksman"))

  ;; Dependency for editing code blocks in markdown-mode.
  (celeste/prepare-package edit-indirect)
  )



(provide 'init-markdown)
;;; init-markdown.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
