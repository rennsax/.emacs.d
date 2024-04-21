;;; init-org.el -- Org: brilliant note-taking system. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Use the builtin `org'
(use-package org
  :init
  (setq org-modules nil) ; Speedup startup.
  (setq org-directory celeste-org-dir)
  (setq org-agenda-files (list org-directory))
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

        ;; Leave a blank line before a new heading. Try C-c RET.
        org-blank-before-new-entry '((heading . always) (plain-list-item . auto))
        ;; Do not use the actual size when inlining the image, i.e. respect
        ;; ATTR_ORG
        org-image-actual-width nil
        )
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)" "CANCEL(c)"))
        org-todo-keyword-faces '(("CANCEL" . error)))

  (celeste/autoload '+org/dwim-at-point org nil t))

(use-package org-goto
  :config
  ;; `org-goto' is a convenient way to navigate an org buffer. However, it
  ;; conflicts with `evil-mode'.
  (advice-add 'org-goto :around
              (lambda (oldfun &rest r)
                (evil-with-state emacs (apply oldfun r)))))

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
         (org-agenda-finalize . org-modern-agenda))
  :config
  ;; Do not hide leading stars of titles. Consequently there are visually
  ;; indentation in the org doc, which indicates the outline levels, making
  ;; the doc more readable.
  (setq org-modern-hide-stars nil))

;; TODO: ob-xx

(celeste/use-package org-roam
  :init
;;; BEGIN Dependencies of `org-roam' 8<
  ;; `dash'
  ;; `f'
  ;; `s'
  ;; `org' (of course)
  ;; `emacsql' and `emacsql-sqlite'
  (add-to-list 'load-path (concat celeste-package-dir "emacsql"))
  (celeste/use-package magit-section
    :load-path "packages/magit/lisp")
  (celeste/use-package filenotify-recursive)
;;; >8 END Dependencies of `org-roam'

  (setq org-roam-directory (concat celeste-org-dir "roam/")
        org-roam-db-location (concat celeste-data-dir "org-roam.db"))

  :commands (org-roam-node-find)
  )

(use-package org-agenda
  :after org
  :init
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  ;; `evil-org-agenda-set-keys' is loaded, `evil-org-agenda-mode' is also
  ;; declared.
  (celeste/autoload 'evil-org-agenda-set-keys org)
  (evil-org-agenda-set-keys)
  ;; Protect the leader key.
  ;; TODO: move to init-keybinding.el.
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd celeste-leader-key) #'celeste-leader-map)
  )


(provide 'init-org)
;;; init-org.el ends here
