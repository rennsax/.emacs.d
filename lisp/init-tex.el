;;; init-tex.el -- Tex editing and BibTeX support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; AUCTeX


(celeste/prepare-package-2 auctex "" :info "doc")
(or (load "auctex-autoloads.el" 'noerror 'nomessage 'must-suffix)
    (warn "It seems that AUCTeX is not installed properly!"))
(use-package latex
  :config
  ;; See `wim-auto-save-mode'.
  (setq TeX-auto-save nil
        TeX-parse-self t)
  (setq TeX-indent-open-delimiters "["
        TeX-indent-close-delimiters "]")
  (setq TeX-save-query nil)

  ;; On macOS, turn on `TeX-source-correlate-mode' (global minor mode). This is
  ;; powered by SyncTeX and Skim PDF reader. AUCTeX will compile the document
  ;; with `--synctex=1' option. `displayline' is a script embedded in the
  ;; Skim.app bundle, you need to put it in `exec-path'.
  (when sys/mac
    (let ((v (assoc 'output-pdf TeX-view-program-selection)))
      (setcdr v '("displayline")))
    (TeX-source-correlate-mode +1)
    (keymap-set TeX-source-correlate-map "s-<mouse-1>" #'TeX-view-mouse))

  ;; Remove some prettify rules in case the editing is too confusing.
  (require 'tex-mode)
  (setq tex--prettify-symbols-alist
        (cl-remove-if (lambda (cell) (string-match-p (rx bos (| "--" "---" "\\newline" "\\qed") eos)
                                                (car-safe cell)))
                      tex--prettify-symbols-alist))
  (add-hook 'LaTeX-mode-hook
            (defun +LaTeX-mode-setup ()
              ;; `$' inserts "\(\)" instead.
              (setq-local TeX-electric-math '("\\( " . " \\)"))
              ;; Symbols in math environment are prettified. Disabled in style files.
              (when (string= (file-name-extension (buffer-file-name)) "tex")
                (prettify-symbols-mode +1))
              (auto-fill-mode +1)
              ;; This will be used by `TeX-command-run-all', bound to "C-c C-a"
              (setq TeX-command-default "LaTeXMk")
              (when (fboundp 'flycheck-mode)
                (flycheck-mode +1))))

  (setq LaTeX-electric-left-right-brace t   ; {} () \left(\right) ...
        TeX-electric-sub-and-superscript t) ; ^{} _{}

  (setq LaTeX-begin-regexp
        (rx (| (seq (| "begin" "While" "For" "ForAll" "Loop" "Repeat" "If" "Procedure" "Function") word-boundary) ?\[))
        LaTeX-end-regexp
        (rx (| (seq (| "end" "EndWhile" "EndFor" "EndForAll" "EndLoop" "Until" "EndIf" "EndProcedure" "EndFunction") word-boundary) ?\]))
        LaTeX-paragraph-commands '("State"))

  ;; Let AUCTeX parse the file to extract customized macros, and make it look
  ;; for as many macros as it can.
  (setq TeX-parse-self t
        TeX-auto-regexp-list TeX-auto-full-regexp-list)

  ;; Always ask for the master file for multi-file document structure. A new TeX
  ;; file will also ask for its master file. Just use C-g to skip.
  (setq-default TeX-master nil)

  ;; After set, persist in local variables.
  (define-advice TeX-engine-set (:after (type) set-file-local-var)
    (if (stringp type) (setq type (intern type)))
    (add-file-local-variable 'TeX-engine type))

  ;; Customize `TeX-command-master':
  (setq TeX-command-list
        (cl-remove-if (lambda (cell) (string= (car-safe cell) "Xindy"))
                      TeX-command-list))
  )

(use-package preview
  :config
  (defun preview-dwim ()
    "Toggle `preview-at-point'."
    (interactive)
    (if (seq-some
         (lambda (ovr) (overlay-get ovr 'preview-state))
         (overlays-in (max (point-min) (1- (point)))
                      (min (point-max) (1+ (point)))))
        (preview-clearout-at-point)
      (preview-at-point)))

  (keymap-set preview-map "p" #'preview-dwim))



;;; BibTeX support

;; citar is brilliant. It helps integrate BibTeX with Emacs.
(celeste/prepare-package parsebib)
(celeste/package-autoload 'citar)
(use-package citar
  :config
  (setq citar-bibliography (list "~/org/bib/My Library.bib"))
  (when sys/mac
    (defmacro +osx-open-with-app (app)
      `(lambda (file)
         (call-process "open" nil 0 nil "-a" ,app file)))
    (add-to-list 'citar-file-open-functions
               `("pdf" . ,(+osx-open-with-app "Preview.app"))))

  ;; `org-mode' integration
  (setq org-cite-global-bibliography citar-bibliography
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)

  ;; Use nerd icons to indicate notes/links/files/cites.
  (with-eval-after-load 'nerd-icons
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-file_o"
                :face 'nerd-icons-green)
       :function #'citar-has-files
       :padding "  " ; need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-link"
                :face 'nerd-icons-orange)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (nerd-icons-codicon
                "nf-cod-note"
                :face 'nerd-icons-blue)
       :function #'citar-has-notes
       :padding "    "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-circle_o"
                :face 'nerd-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))

    (setq citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-links-icons
                citar-indicator-notes-icons
                citar-indicator-cited-icons))
    )
  )

(use-package citar-capf
  :init
  (define-advice citar-capf-setup (:override () buffer-locally)
    "Only add `citar-capf' buffer-locally."
    (add-hook 'completion-at-point-functions 'citar-capf nil t))
  :hook (((org-mode LaTeX-mode) . citar-capf-setup)))

(use-package citar-embark
  :diminish
  :after citar embark
  :no-require
  :demand t
  :config (citar-embark-mode))

;; Use `org-roam' to add notes.
(use-package citar-org-roam
  :after citar
  :diminish
  :init (celeste/prepare-package citar-org-roam)
  :demand t
  :config
  (setq citar-org-roam-subdir "citar"
        citar-org-roam-note-title-template "${title}")
  (citar-org-roam-mode))



(provide 'init-tex)
;;; init-tex.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
