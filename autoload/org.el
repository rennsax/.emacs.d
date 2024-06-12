;;; org.el -- Org extra subroutines. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; From doom. Big thanks!!

;;;###autoload
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


;;; My customized subroutines.

;;;###autoload
(defun +org-cut-this-link ()
  "If the cursor is on an org-style link, kill it."
  (interactive)
  (if-let ((match (org-in-regexp org-link-bracket-re 1)))
      (apply #'kill-region (list (car match) (cdr match)))
    (user-error "Did not find an org-style link!")))

;;;###autoload
(defun +org-convert-link-to-description ()
  "Remove hyperlink but keep the description."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((link-region (list (match-beginning 0) (match-end 0)))
              (description
               (match-string-no-properties (if (match-end 2) 2 1))))
          (apply #'delete-region link-region)
          (insert description)))
    (user-error "Did not find an org-style link!")))

;;;###autoload
(defun +org-transfer-this-markdown-link ()
  "Transfer the markdown-style link at the current point."
  (interactive)
  (let ((md-inline-link-re "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\s-*\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\s-*\\(?8:)\\)"))
    (if (org-in-regexp md-inline-link-re 1)
        (replace-match "[[\\6][\\3]]")
      (message "Not a markdown-style link!"))))

;;;###autoload
(defun +org-sort-todo-with-priority ()
  "First sort by priority, then sort by todo."
  (interactive)
  (org-sort-entries nil ?p)
  (org-sort-entries nil ?o)
  (org-ctrl-c-tab))

;;;###autoload
(defun +org-toggle-emphasis ()
  "Toggle emphasis."
  (interactive)
  (save-match-data
    (if (and (or (org-in-regexp org-emph-re 2)
                 (org-in-regexp org-verbatim-re 2))
             (not (region-active-p)))
        (let ((beg (match-beginning 3))
              (end (match-end 4)))
          (when (and (>= (point) (1- beg))
                     (<= (point) (1+ end)))
            (save-excursion
              (goto-char end)
              (delete-char 1)
              (goto-char beg)
              (delete-char 1))))
      (call-interactively #'org-emphasize))))

;;;###autoload
(defun +Info-copy-current-node-as-org-link ()
  (interactive nil Info-mode)
  (unless Info-current-node
    (user-error "No current Info node"))
  (require 'ol)
  (let* ((node (if (stringp Info-current-file)
                           (concat "(info \"(" (file-name-sans-extension
                                                    (file-name-nondirectory Info-current-file))
                                       ") "
                                       Info-current-node
                           "\")")
                 (format "(Info-find-node '%S '%S)"
                                     Info-current-file Info-current-node)))
         (link-string (org-link-make-string (concat "elisp:" node)
                                            (concat "Info: " Info-current-node))))
    (kill-new link-string)
    (message "%s" link-string)))

;; Local Variables:
;; no-byte-compile: t
;; End:
