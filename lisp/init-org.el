;;; init-org.el -- Org: brilliant note-taking system. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Install external org-mode, which includes newer features.
;; 2024-05-30: I found `org-agenda-prepare-window' will delete all other windows
;; before pop to the agenda buffer, which is really weird. The newer version of
;; org-mode has changed the behavior, so `display-buffer-alist' is better
;; respected. This enhancement also convinces myself to stick to a newer version
;; of org-mode.
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

  ;; Hack the org syntax table.
  (modify-syntax-entry ?< "w" org-mode-syntax-table)
  (modify-syntax-entry ?> "w" org-mode-syntax-table)

  ;; `org-return' is really newline (w/o indentation), and
  ;; `org-return-and-maybe-indent' is newline w/ indentation. They are bound to
  ;; RET and C-j, by default.
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
  ;; The doc for `org-return-and-maybe-indent' is somehow wrong - it has nothing
  ;; to do with `org-adapt-indentation'.
  (setq org-adapt-indentation nil)

  (setq org-ellipsis "⤵"
        org-cycle-separator-lines 0)  ; never leave empty lines in collapsed view

  (setq org-hide-leading-stars nil)

  ;; Place tags directly after headline text, with only one space in between.
  (setq org-tags-column 0
        ;; Leave a blank line before a new heading. Try C-c RET.
        org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

  ;; The default todo keywords also specify that log the time when the todo
  ;; status changes to DONE or CANCELED. I recommend to check the option
  ;; `org-log-into-drawer'.
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "PENDING(p)" "|" "DONE(d!)" "CANCELED(c@)"))
        org-todo-keyword-faces '(("CANCELED" . (:foreground "#ff5630" :strike-through t))
                                 ("DOING" . (:foreground "#ffc600" :weight bold))
                                 ("PENDING" . (:foreground "#a3b18a"))) ; teal color
        ;; If dependencies are not done, forbid to mark TODO entries as DONE.
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        ;; No special window will be shown.
        org-use-fast-todo-selection 'expert)

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

  ;; org-id behavior: if interactively call `org-store-link' in an org file, try
  ;; to create custom id if the target entry has no.
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

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

  (setq org-file-apps '((auto-mode . emacs)
                        (directory . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . emacs))) ; Open pdf with doc-view

  ;; TODO: use general autoload interface.
  (load (concat celeste-autoload-dir "org.el") nil nil t)
  (keymap-set org-mode-map "s-<return>" #'+org/dwim-at-point)

  (keymap-unset org-mode-map "C-'" t)

  (bind-keys ("C-c o l" . org-store-link))
  )

(use-package org-agenda
  :init
  (bind-keys ("C-c o A" . org-agenda)
             ("C-c o a" . org-agenda-list))
  :config
  (setq org-agenda-window-setup 'current-window)
  )

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

(use-package org-refile
  :config
  (setq org-refile-targets `((nil . (:maxlevel . 9))) ; consider *all* heading in the current buffer
        org-outline-path-complete-in-steps nil        ; select completion at one time
        org-refile-use-outline-path t)                ; select target like paths
  )


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

;; Alert
(use-package alert
  :init (celeste/prepare-package alert)
  :commands alert
  :config

  ;; Set alert style.
  (cond
   (sys/mac
    (setq alert-default-style 'osx-notifier))
   (sys/linux
    ;; --with-dbus
    (setq alert-default-style 'notifier)))

  ;; FIXED: encoding CJK characters leads to Apple Script error.
  (when sys/mac
    (fset #'alert-osx-notifier-notify
          (defun +cjk-fix-alert-osx-notifier-notify (info)
            (do-applescript (format "display notification %S with title %S"
                                    ;; Why we need the encoding?
                                    (plist-get info :message)
                                    (plist-get info :title)))
            (alert-message-notify info)))))

(use-package org-alert
  :init (celeste/prepare-package org-alert)
  :after org-agenda
  :demand t
  :config
  (setq org-alert-notification-title "Org Agenda"
        ;; Check agendas each 5 minutes.
        org-alert-interval 300
        org-alert-cutoff-prop "REMINDERN")
  (org-alert-enable)

  ;; Ignore DOING entries.
  (setq org-alert-match-string
        (format "SCHEDULED>=%S+SCHEDULED<%S-todo=%S|DEADLINE>=%S+DEADLINE<%S-todo=%S"
                "<today>" "<tomorrow>" "DOING" "<today>" "<tomorrow>" "DOING"))
  )


;; Journal
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


(use-package org-roam
  :init
  ;; `dash'
  ;; `f'
  ;; `s'
  ;; `org' (of course)
  ;; `magit-section'
  ;; `emacsql' and `emacsql-sqlite'
  (celeste/prepare-package (dash f s emacsql filenotify-recursive))
  (celeste/prepare-package-2 org-roam "" "extensions" :info "doc")

  ;; I'm tried of remembering the order of verb and noun.
  (defalias 'org-roam-find-node 'org-roam-node-find)
  (defalias 'org-roam-random-node 'org-roam-node-random)
  (defalias 'org-roam-insert-node 'org-roam-node-insert)
  (defalias 'org-roam-open-node 'org-roam-node-open)
  (defalias 'org-roam-visit-node 'org-roam-node-visit)

  (setq org-roam-directory (concat celeste-org-dir "roam/")
        org-roam-db-location (concat celeste-data-dir "org-roam.db"))

  :bind (("C-c o r n" . org-roam-node-find)
         ("C-c o r s" . org-roam-db-sync)
         ("C-c o r i" . org-roam-node-insert))
  :config

  ;; Use the standard `display-buffer'.
  (fset #'org-roam-id-open
        (defun +org-roam-id-open-with-display-buffer (id _)
          "Go to the entry with id ID.
Like `org-id-open', but additionally uses the Org-roam database."
          (org-mark-ring-push)
          (let ((m (or (org-roam-id-find id 'marker)
                       (org-id-find id 'marker))))
            (unless m
              (error "Cannot find entry with ID \"%s\"" id))
            (if (not (equal (current-buffer) (marker-buffer m)))
                (display-buffer (marker-buffer m)))
            (goto-char m)
            (move-marker m nil)
            (org-show-context))))
  )

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

;; Automatically toggle Org mode LaTeX fragment previews
(use-package org-fragtog
  :init (celeste/prepare-package org-fragtog)
  :hook (org-mode . org-fragtog-mode))



(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
