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
  ;; Originally `org-cycle-agenda-files'.
  (keymap-unset org-mode-map "C-'" t)
  (bind-keys ("C-c o l" . org-store-link))

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

  ;; The curly braces are *required* in order to trigger interpretations as
  ;; sub/superscript. See also `org-export-with-sub-superscripts'.
  (setq org-use-sub-superscripts '{})

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

  )

(use-package org-agenda
  :init
  (bind-keys ("C-c o A" . org-agenda))
  :config
  ;; "o" is mapped to `delete-other-windows' in `org-agenda-mode', but I think
  ;; it's unnecessary.
  (keymap-unset org-agenda-keymap "o" t)

  ;; How the agenda buffer is displayed
  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit nil)

  ;; Do not show DONE entries!
  (setq org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t)

  (setq org-agenda-skip-scheduled-if-deadline-is-shown nil
        org-agenda-skip-timestamp-if-deadline-is-shown nil
        org-agenda-skip-deadline-prewarning-if-scheduled t)

  ;; Skip useless subtrees.
  (setq org-agenda-skip-comment-trees t
        org-agenda-skip-archived-trees t)

  ;; Deadline is just deadline. Do not dim it!
  (setq org-agenda-dim-blocked-tasks nil)

  ;; `org-agenda-follow-mode' displays only the current item's tree.
  (setq org-agenda-follow-indirect t)

  ;; Otherwise, the buffer's relative position will change in the window.
  (define-advice org-agenda-redo-all (:around (oldfun &rest args) move-to-begin)
    (let ((cur-win (current-window-configuration)))
      (apply oldfun args)
      (set-window-configuration cur-win)))

  )

(use-package org-capture
  :init
  (bind-keys ("C-c o c" . org-capture)))

(use-package org-indent
  :diminish org-indent-mode
  :config
  (setq org-indent-mode-turns-on-hiding-stars nil))

(use-package org-keys
  :config
  (setq org-use-speed-commands t))

(use-package org-refile
  :config
  (setq org-refile-targets '((nil . (:maxlevel . 9)) ; consider *all* heading in the current buffer
                             (org-agenda-files . (:level . 1))) ; allow agenda files
        org-outline-path-complete-in-steps nil        ; select completion at one time
        org-refile-use-outline-path t)                ; select target like paths
  )

(use-package org-protocol
  :init
  ;; Lazy-load `org-protocol'.
  (advice-add 'server-visit-files :around #'org--protocol-detect-protocol-server)
  :autoload org--protocol-detect-protocol-server)


;;; ox: Export Framework for Org Mode

(use-package ox
  :config
  (setq org-export-with-sub-superscripts '{}
        org-export-with-section-numbers nil)
  )

;; Use "listings" as the LaTeX backend for source block.
(use-package ox-latex
  :config
  (setq org-latex-src-block-backend 'listings)
  (setq org-latex-packages-alist
        '(("" "listings")))
  ;; Remove "capt-of" from the default package list since its use case is too
  ;; narrow.
  (setq org-latex-default-packages-alist
        (cl-remove-if (lambda (cell) (string= (cadr cell) "capt-of"))
                      org-latex-default-packages-alist)))

(use-package ox-html
  :config
  ;; org-mode now leverage htmlize.el to fontify the source block.
  (celeste/prepare-package htmlize)

  (setq org-html-postamble nil
        org-html-preamble nil)
  )

(use-package ox-gfm
  :after ox-md
  :init (celeste/prepare-package ox-gfm)
  :demand t
  :config
  (define-advice org-gfm-src-block (:override (src-block _contents info) transform-lang)
    "Transcode SRC-BLOCK element into Github Flavored Markdown format.

Also respect `org-blackfriday-syntax-highlighting-langs'."
    (let* ((lang (org-element-property :language src-block))
           ;; This line is added.
           (lang (or (cdr (assoc lang org-blackfriday-syntax-highlighting-langs)) lang))
           (code (org-export-format-code-default src-block info))
           (prefix (concat "```" lang "\n"))
           (suffix "```"))
      (concat prefix code suffix))))



;;; third-party

(use-package ox-hugo
  :after ox
  :demand t
  :disabled t
  :init
  (celeste/prepare-package (tomelr ox-hugo))
  :config
  (setq org-hugo-use-code-for-kbd t)
  ;; Hugo uses Goldmark to render Markdown to HTML, which treats `_' and `*'
  ;; differently. `a_bb_c' does not make `bb' italic, but `a*bb*c' makes it italic.
  (define-advice org-blackfriday-italic (:override (_italic contents _info) asterisk)
    (format "*%s*" contents))
  )

;; `ox-blackfriday' is included in `ox-hugo'.
(use-package ox-blackfriday
  :init (celeste/prepare-package ox-hugo)
  :after ox
  :demand t
  :config
  ;; Handle treesit major modes.
  (setq org-blackfriday-syntax-highlighting-langs
        (append org-blackfriday-syntax-highlighting-langs
                '(("dockerfile-ts" . "dockerfile")
                  ("cmake-ts" . "cmake"))))

  ;; Add blackfriday format to `org-export-dispatch'.
  (eval
   '(let ((backend (org-export-get-backend 'blackfriday)))
      (setf (org-export-backend-menu backend)
            '(?b "Export to Blackfriday Markdown"
                 ((?B "To a temporary Md buffer"
                      (lambda (a s v _b)
                        (org-blackfriday-export-as-markdown a s v)))
                  (?b "To Md file"
                      (lambda (a s v _b)
                        (org-blackfriday-export-to-markdown a s v)))
                  (?o "To file and open"
                      (lambda (a s v _b)
                        (if a
                            (org-blackfriday-export-to-markdown :async s v)
                          (org-open-file (org-blackfriday-export-to-markdown nil s v)))))
                  )))))
  )

(use-package org-hugo-auto-export-mode
  :disabled t
  :init
  (use-package ox-hugo
    :commands org-hugo-export-wim-to-md)
  :commands org-hugo-auto-export-mode)

(use-package org-super-agenda
  :after org-agenda
  ;; Immediately loaded after `org-agenda'.
  :demand t
  :init
  (celeste/prepare-package-2 (ts ht (org-super-agenda :load-path "" :info "")))

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
  :init
  ;; (celeste/package-build-autoload 'org-journal)
  ;; REVIEW: `org-loaddefs' does not autoload `org-element-type'.
  (autoload 'org-element-type "org-element-ast")
  (celeste/package-autoload 'org-journal)
  (setq org-journal-dir (concat celeste-org-dir "journal"))

  :config
  (setq org-journal-file-type 'weekly
        org-journal-date-format "%A, %B %d %Y")

  (defun org-journal-file-header-fn (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))
  (setq org-journal-file-header #'org-journal-file-header-fn)
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
        org-roam-db-location (celeste/make-path "org-roam.db" 'data))

  :bind (("C-c o r s" . org-roam-db-sync)
         ("C-c o n" . org-roam-node-find))
  :config
  ;; After loading, synchronize the org-roam-db cache.
  (org-roam-db-sync)

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

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    "Method to get the parent directory of NODE."
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (split-string dirs "/")))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    "Method to get the backlink count of NODE."
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  ;; `marginalia' should be loaded.
  (setq org-roam-node-display-template
        (concat (propertize "${tags:16}" 'face 'org-tag)
                (propertize "${directories:12}" 'face 'org-priority)
                " ${title}       ")
        org-roam-node-annotation-function
        (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))

  ;; Includes all sections in `org-roam-buffer'.
  (setq org-roam-mode-sections
        '(org-roam-backlinks-section
          org-roam-reflinks-section
          org-roam-unlinked-references-section))

  ;; Embark integration.
  (with-eval-after-load 'embark
    (defvar-keymap embark-org-roam-map
      :doc "Keymap for Embark org roam actions."
      "i" #'org-roam-node-insert)
    (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map)))

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
  (setq org-download-image-html-width "80%")
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

(use-package org-pandoc-import
  :init (celeste/prepare-package org-pandoc-import)
  :commands (org-pandoc-import-as-org
             org-pandoc-import-to-org))



(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
