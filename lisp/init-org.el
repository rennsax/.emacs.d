;;; init-org.el -- Org: brilliant note-taking system. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Use the builtin `org'
(use-package org
  :init
  (setq org-modules nil) ; Speedup startup.
  (setq org-directory celeste-org-dir)
  (setq org-agenda-files (list (concat org-directory "agenda")))

  :bind (("C-c o A" . org-agenda)
         ("C-c o a" . org-agenda-list))

  :config
  (setq org-edit-src-content-indentation 0
        org-hide-leading-stars t
        ;; Also fontify code in code blocks.
        org-src-fontify-natively t
        ;; TAB uses the language’s major-mode binding in code blocks.
        org-src-tab-acts-natively t
        ;; Leading whitespace are not preserved on export, and when switching
        ;; between the org buffer and the language mode edit buffer.
        org-src-preserve-indentation nil
        org-fontify-quote-and-verse-blocks t
        ;; REVIEW: I personally disable indentation view because of:
        ;; 1. Performance.
        ;; 2. Many things are buggy when interacting with `org-indent-mode'.
        ;; There may be some hacks, but I'm tried to find those resolutions. For
        ;; instance, see https://github.com/minad/org-modern/issues/7.
        ;; 3. These visual indentation is dropped after export.
        ;; org-startup-indented t

        ;; Place tags directly after headline text, with only one space in between.
        org-tags-column 0

        ;; Leave a blank line before a new heading. Try C-c RET.
        org-blank-before-new-entry '((heading . always) (plain-list-item . auto))
        ;; Do not use the actual size when inlining the image, i.e. respect
        ;; ATTR_ORG
        org-image-actual-width nil
        )
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCEL(c)"))
        org-todo-keyword-faces '(("CANCEL" . error)))

  (celeste/autoload '+org-toggle-inline-images-in-subtree org nil t)
  (celeste/autoload '+org/dwim-at-point org nil t)
  (keymap-set org-mode-map "s-<return>" #'+org/dwim-at-point)

  ;; Resolve keymap conflicts.
  (when (fboundp 'avy-goto-char-2)
    (keymap-unset org-mode-map "C-'"))
  )

(use-package org-id
  :init
  (setq org-id-locations-file (file-name-concat org-directory ".org-id-locations")))

;; Use "listings" as the LaTeX backend for source block.
(use-package ox-latex
  :after org
  :config
  (setq org-latex-src-block-backend 'listings)
  (setq org-latex-packages-alist
        '(("" "listings"))))

;; Modern org style. Why are you so energetic, dear minad?
(celeste/use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         ;; Also prettify the agenda buffer.
         (org-agenda-finalize . org-modern-agenda)))

;; TODO: ob-xx

(celeste/use-package org-roam
  :defines (org-roam-directory
            org-roam-db-location)
  :functions org-roam-db-autosync-enable
  :init
;;; BEGIN Dependencies of `org-roam' 8<
  ;; `dash'
  ;; `f'
  ;; `s'
  ;; `org' (of course)
  ;; `magit-section'
  ;; `emacsql' and `emacsql-sqlite'
  (add-to-list 'load-path (concat celeste-package-dir "emacsql"))
  (celeste/use-package filenotify-recursive)
;;; >8 END Dependencies of `org-roam'

  (setq org-roam-directory (concat celeste-org-dir "roam/")
        org-roam-db-location (concat celeste-data-dir "org-roam.db"))

  :bind (("C-c o r n" . org-roam-node-find))
  :config
  ;; Necessary for automatic `org-roam-complete-link-at-point'
  (org-roam-db-autosync-enable))

(use-package org-agenda
  :after org
  :config

  ;; Do not destroy my window layout!!!
  (if (featurep 'popper)
      (setq org-agenda-window-setup 'other-window)
    (setq org-agenda-window-setup 'current-window))
  )

(celeste/use-package org-super-agenda
  :after org-agenda
  ;; Immediately loaded after `org-agenda'.
  :demand t
  :init
  ;; Emacs timestamp and date-time library.
  (celeste/require 'ts)
  ;; Emacs hashtable library.
  (celeste/require 'ht)
  :commands org-super-agenda-mode
  :config
  ;; Global minor mode. From now on, `org-agenda-list' respects a series of
  ;; variables from `org-super-agenda', e.g. `org-super-agenda-groups'.
  (org-super-agenda-mode))


(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
