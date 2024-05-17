;;; init-org.el -- Org: brilliant note-taking system. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Install external org-mode, which includes newer features.
(celeste/prepare-package-2 org-mode "lisp" :info "doc")
(require 'org-loaddefs)

(use-package org
  :init
  (setq org-modules nil) ; Speedup startup.
  (setq org-directory celeste-org-dir)
  (setq org-agenda-files (list (concat org-directory "agenda")))
  (setq org-id-locations-file (file-name-concat org-directory ".meta" ".org-id-locations"))

  ;; Ensure necessary directories exist.
  (dolist (subdir '(".meta" "agenda" "roam" "journal"))
    (make-directory (concat org-directory subdir) t))

  :config
  (setq org-ellipsis "⤵"
        org-cycle-separator-lines 0  ; never leave empty lines in collapsed view
        )
  (setq org-hide-leading-stars nil)

  ;; Place tags directly after headline text, with only one space in between.
  (setq org-tags-column 0
        ;; Leave a blank line before a new heading. Try C-c RET.
        org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCELED(c)"))
        org-todo-keyword-faces '(("CANCELED" . error)
                                 ("DOING" . warning))
        ;; If dependencies are not done, forbid to mark TODO entries as DONE.
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;; Add a special face for quote and verse.
  (setq org-fontify-quote-and-verse-blocks t)

  ;; Do not use the actual size when inlining the image, i.e. respect ATTR_ORG
  (setq org-image-actual-width nil)

  ;; Also fontify code in code blocks.
  (setq org-src-fontify-natively t
        ;; TAB uses the language’s major-mode binding in code blocks.
        org-src-tab-acts-natively t
        ;; Leading whitespace are not preserved on export, and when switching
        ;; between the org buffer and the language mode edit buffer.
        org-src-preserve-indentation nil
        ;; No extra indent for src block
        org-edit-src-content-indentation 0
        )

  ;; When the cursor is on a link, `newline' is not what you want generally. If
  ;; you do want to "insert a newline", consider "C-o" (`org-open-line').
  (setq org-return-follows-link t)

  ;; Customize org-bold face.
  (progn
    (defface org-bold '((default :inherit bold)) "My bold emphasis for Org.")

    (setq org-emphasis-alist
          '(("*" org-bold)
            ("/" italic)
            ("_" underline)
            ("=" org-verbatim verbatim)
            ("~" org-code verbatim)
            ("+" (:strike-through t)))))

  ;; TODO: use general autoload interface.
  (load (concat celeste-autoload-dir "org.el") nil nil t)
  (keymap-set org-mode-map "s-<return>" #'+org/dwim-at-point)

  (keymap-unset org-mode-map "C-'" t)

  (bind-keys ("C-c o l" . org-store-link))
  )

(use-package org-agenda
  :init
  (bind-keys ("C-c o A" . org-agenda)
             ("C-c o a" . org-agenda-list)))

(use-package org-capture
  :init
  (bind-keys ("C-c o c" . org-capture)))

;; Use "listings" as the LaTeX backend for source block.
(use-package ox-latex
  :config
  (setq org-latex-src-block-backend 'listings)
  (setq org-latex-packages-alist
        '(("" "listings"))))

(use-package org-indent
  :diminish org-indent-mode
  :config
  (setq org-indent-mode-turns-on-hiding-stars nil))

(use-package org-keys
  :config
  (setq org-use-speed-commands t))


;;; third-party

(use-package org-super-agenda
  :after org-agenda
  ;; Immediately loaded after `org-agenda'.
  :demand t
  :init
  ;; Emacs timestamp and date-time library.
  (celeste/prepare-package ts)
  ;; Emacs hashtable library.
  (celeste/prepare-package ht)
  (celeste/prepare-package org-super-agenda)

  :commands org-super-agenda-mode
  :config
  ;; Global minor mode. From now on, `org-agenda-list' respects a series of
  ;; variables from `org-super-agenda', e.g. `org-super-agenda-groups'.
  (org-super-agenda-mode))

(use-package org-journal
  :after org-capture
  :commands org-journal-new-entry
  :init
  (celeste/prepare-package org-journal)

  (setq org-journal-dir (concat celeste-org-dir "journal"))
  :config
  (setq org-journal-file-type 'weekly
        org-journal-date-format "%A, %B %d %Y")

  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
  (setq org-journal-file-header #'org-journal-file-header-func)
  )

(celeste/prepare-package-2 org-roam "" "extensions" :info "doc")

(use-package org-roam
  :init
  ;; `dash'
  ;; `f'
  ;; `s'
  ;; `org' (of course)
  ;; `magit-section'
  ;; `emacsql' and `emacsql-sqlite'
  (celeste/prepare-package dash)
  (celeste/prepare-package emacsql)
  (celeste/prepare-package filenotify-recursive)

  (setq org-roam-directory (concat celeste-org-dir "roam/")
        org-roam-db-location (concat celeste-data-dir "org-roam.db"))

  :bind (("C-c o r n" . org-roam-node-find))
  :config
  ;; Necessary for automatic `org-roam-complete-link-at-point'
  (org-roam-db-autosync-enable))

(use-package org-download
  :init
  (celeste/prepare-package async)
  (celeste/prepare-package org-download)

  :commands org-download-screenshot org-download-clipboard
  :config
  (setq org-download-backend "curl \"%s\" -o \"%s\""
        org-download-method 'directory)
  (setq org-download-display-inline-images nil)
  (setq-default org-download-heading-lvl nil)
  (when sys/mac
    (setq org-download-screenshot-method "screencapture -i %s"))

  (defcustom org-download-no-new-dir
    nil
    "If true, `org-download--dir-2' always return \"\"."
    :type 'boolean)
  (make-variable-buffer-local 'org-download-no-new-dir)
  (advice-add 'org-download--dir-2 :override
              (defun +org-download--dir-typora-style ()
                (if org-download-no-new-dir ""
                  (let* ((file-name (buffer-file-name (buffer-base-buffer)))
                         (base-name (if file-name (file-name-nondirectory file-name) "_")))
                    (concat base-name ".assets")))))
  )


(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
