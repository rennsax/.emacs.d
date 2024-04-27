;;; From doom. Big thanks!!
;; TODO: a) use autoload, b) fork Somelauw/evil-org-mode

(defun +org-toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree.

If BEG and END is nil, by default [BEG, END] refers to the
current subtree.

If REFERSH is non-nil, always refresh the inline images. By
default inline images are toggled."
  (interactive)
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         ;; Get `org-image-overlay' overlays in [BEG, END]
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    ;; Remove previous image overlays.
    (dolist (ov overlays nil)
      ;; `delete-overlay' actually affects the display.
      (delete-overlay ov)
      ;; Also, set the `org-mode' inner variable `org-inline-image-overlays' so
      ;; that things can work properly. See also `org-remove-inline-images'.
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    ;; If REFERSH is non-nil, or there is no previous `org-image-overlay', then
    ;; re-display inline images.
    (when (or refresh (not overlays))
      ;; INCLUDE-LINKED: also inline those links with description. By default
      ;; they are not images but plain links that can be exported. The option is
      ;; nice for a quick look at the images.
      (org-display-inline-images 'include-linked 'refresh beg end)
      t)))

;;;###autoload
(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If ARG is non-nil, display information verbosely.

If on a:
- checkbox list item or todo heading: toggle it.
- link: follow it
- inline image: toggle display"

  (interactive "P")
  (let* ((context (org-element-context))
         (type (org-element-type context)))

    ;; switch .. case ..
    (pcase type
      ('link
       (let ((path (org-element-property :path context)))
         (if (or (equal (org-element-property :type context) "img")
                 ;; Or Emacs has native support for displaying the image.
                 (and path (image-supported-file-p path)))
             (+org-toggle-inline-images-in-subtree
              ;; Only display the current image.
              (org-element-property :begin context)
              (org-element-property :end context))
           ;; Not an image, just open it.
           (org-open-at-point arg))))
      ;; Checkbox.
      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (org-toggle-checkbox))
      (_
       (if arg
           (message "Unknown context: %s" context)
         (message "Undefined dwim action."))
       ))))

;;; doomelpa/evil-org-mode

(defvar evil-org-agenda-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode evil-org-agenda-mode
  "Buffer local minor mode for evil-org-agenda"
  :init-value nil
  :lighter " EvilOrgAgenda"
  :keymap evil-org-agenda-mode-map
  :group 'evil-org
  (when evil-org-agenda-mode
    (evil-normalize-keymaps)))

;;;###autoload
(defun evil-org-agenda-set-keys ()
  "Set motion state keys for `org-agenda'."
  (evil-set-initial-state 'org-agenda-mode 'motion)


  ;; Horizontal movements have little use, thus we can override "f" and "t".
  ;; "w", "b", "e", "ge" and their upcase counterparts are preserved.
  (evil-define-key 'motion evil-org-agenda-mode-map
    ;; Unused keys: D, X


    ;; open
    (kbd "<tab>") 'org-agenda-goto
    (kbd "S-<return>") 'org-agenda-goto
    (kbd "g TAB") 'org-agenda-goto
    (kbd "RET") 'org-agenda-switch-to
    (kbd "M-RET") 'org-agenda-recenter


    (kbd "SPC") 'org-agenda-show-and-scroll-up
    (kbd "<delete>") 'org-agenda-show-scroll-down
    (kbd "<backspace>") 'org-agenda-show-scroll-down


    ;; motion
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    "gj" 'org-agenda-next-item
    "gk" 'org-agenda-previous-item
    "gH" 'evil-window-top
    "gM" 'evil-window-middle
    "gL" 'evil-window-bottom
    (kbd "C-j") 'org-agenda-next-item
    (kbd "C-k") 'org-agenda-previous-item
    (kbd "[[") 'org-agenda-earlier
    (kbd "]]") 'org-agenda-later


    ;; manipulation
    ;; We follow standard org-mode bindings (not org-agenda bindings):
    ;; <HJKL> change todo items and priorities.
    ;; M-<jk> drag lines.
    ;; M-<hl> cannot demote/promote, we use it for "do-date".
    "J" 'org-agenda-priority-down
    "K" 'org-agenda-priority-up
    "H" 'org-agenda-do-date-earlier
    "L" 'org-agenda-do-date-later
    "t" 'org-agenda-todo
    (kbd "M-j") 'org-agenda-drag-line-forward
    (kbd "M-k") 'org-agenda-drag-line-backward
    (kbd "C-S-h") 'org-agenda-todo-previousset ; Original binding "C-S-<left>"
    (kbd "C-S-l") 'org-agenda-todo-nextset ; Original binding "C-S-<right>"


    ;; undo
    "u" 'org-agenda-undo


    ;; actions
    "dd" 'org-agenda-kill
    "dA" 'org-agenda-archive
    "da" 'org-agenda-archive-default-with-confirmation
    "ct" 'org-agenda-set-tags
    "ce" 'org-agenda-set-effort
    "cT" 'org-timer-set-timer
    "i" 'org-agenda-diary-entry
    "a" 'org-agenda-add-note
    "A" 'org-agenda-append-agenda
    "C" 'org-agenda-capture


    ;; mark
    "m" 'org-agenda-bulk-toggle
    "~" 'org-agenda-bulk-toggle-all
    "*" 'org-agenda-bulk-mark-all
    "%" 'org-agenda-bulk-mark-regexp
    "M" 'org-agenda-bulk-unmark-all
    "x" 'org-agenda-bulk-action


    ;; refresh
    "gr" 'org-agenda-redo
    "gR" 'org-agenda-redo-all


    ;; quit
    "ZQ" 'org-agenda-exit
    "ZZ" 'org-agenda-quit


    ;; display
    ;; "Dispatch" can prefix the following:
    ;; 'org-agenda-toggle-deadlines
    ;; 'org-agenda-toggle-diary
    ;; 'org-agenda-follow-mode
    ;; 'org-agenda-log-mode
    ;; 'org-agenda-entry-text-mode
    ;; 'org-agenda-toggle-time-grid
    ;; 'org-agenda-day-view
    ;; 'org-agenda-week-view
    ;; 'org-agenda-year-view
    "gD" 'org-agenda-view-mode-dispatch
    "ZD" 'org-agenda-dim-blocked-tasks


    ;; filter
    "sc" 'org-agenda-filter-by-category
    "sr" 'org-agenda-filter-by-regexp
    "se" 'org-agenda-filter-by-effort
    "st" 'org-agenda-filter-by-tag
    "s^" 'org-agenda-filter-by-top-headline
    "ss" 'org-agenda-limit-interactively
    "S" 'org-agenda-filter-remove-all


    ;; clock
    "I" 'org-agenda-clock-in ; Original binding
    "O" 'org-agenda-clock-out ; Original binding
    "cg" 'org-agenda-clock-goto
    "cc" 'org-agenda-clock-cancel
    "cr" 'org-agenda-clockreport-mode


    ;; go and show
    "." 'org-agenda-goto-today ; TODO: What about evil-repeat?
    "gc" 'org-agenda-goto-calendar
    "gC" 'org-agenda-convert-date
    "gd" 'org-agenda-goto-date
    "gh" 'org-agenda-holidays
    "gm" 'org-agenda-phases-of-moon
    "gs" 'org-agenda-sunrise-sunset
    "gt" 'org-agenda-show-tags


    "p" 'org-agenda-date-prompt
    "P" 'org-agenda-show-the-flagging-note


    ;; 'org-save-all-org-buffers ; Original binding "C-x C-s"


    ;; Others
    "+" 'org-agenda-manipulate-query-add
    "-" 'org-agenda-manipulate-query-subtract))

;;;###autoload
(defun +org-cycle-only-current-subtree-h (&optional arg)
  "Toggle the local fold at the point, and no deeper.
`org-cycle's standard behavior is to cycle between three levels: collapsed,
subtree and whole document. This is slow, especially in larger org buffer. Most
of the time I just want to peek into the current subtree -- at most, expand
*only* the current subtree.

All my (performant) foldings needs are met between this and `org-show-subtree'
(on zO for evil users), and `org-cycle' on shift-TAB if I need it."
  (interactive "P")
  (unless (or (eq this-command 'org-shifttab)
              (and (bound-and-true-p org-cdlatex-mode)
                   (or (org-inside-LaTeX-fragment-p)
                       (org-inside-latex-macro-p))))
    (save-excursion
      (org-beginning-of-line)
      (let (invisible-p)
        (when (and (org-at-heading-p)
                   (or org-cycle-open-archived-trees
                       (not (member org-archive-tag (org-get-tags))))
                   (or (not arg)
                       (setq invisible-p
                             (memq (get-char-property (line-end-position)
                                                      'invisible)
                                   '(outline org-fold-outline)))))
          (unless invisible-p
            (setq org-cycle-subtree-status 'subtree))
          (org-cycle-internal-local)
          t)))))

;;; Folds
;;;###autoload
(defalias #'+org/toggle-fold #'+org-cycle-only-current-subtree-h)

;;;###autoload
(defun +org/open-fold ()
  "Open the current fold (not but its children)."
  (interactive)
  (+org/toggle-fold t))

;;;###autoload
(defalias #'+org/close-fold #'outline-hide-subtree)

;;;###autoload
(defun +org/close-all-folds (&optional level)
  "Close all folds in the buffer (or below LEVEL)."
  (interactive "p")
  (outline-hide-sublevels (or level 1)))

;;;###autoload
(defun +org/open-all-folds (&optional level)
  "Open all folds in the buffer (or up to LEVEL)."
  (interactive "P")
  (if (integerp level)
      (outline-hide-sublevels level)
    (outline-show-all)))

(defun +org--get-foldlevel ()
  (let ((max 1))
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (org-next-visible-heading 1)
          (when (memq (get-char-property (line-end-position)
                                         'invisible)
                      '(outline org-fold-outline))
            (let ((level (org-outline-level)))
              (when (> level max)
                (setq max level))))))
      max)))
