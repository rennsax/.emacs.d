;;; init-dired.el -- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Emacs dired - practical file explorer 📂
(use-package dired
  :bind (:map dired-mode-map
              ("DEL" . dired-up-directory))
  :config
  ;; Use the dir in another dired window as the default dir rather than the
  ;; current one.
  (setq dired-dwim-target t)
  ;; Copies and deletes are recursive, of course.
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)

  (setq dired-auto-revert-buffer t)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; On macOS, try to use GNU ls. If not found, use the builtin `ls-lisp',
  ;; TODO: MS Windows?
  (when sys/mac
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls"))
      (progn
        (defvar ls-lisp-support-shell-wildcards nil)
        ;; The package advices `insert-directory' to implement it directly from
        ;; Emacs Lisp, w/o running ls in a subprocess.
        (require 'ls-lisp)))))

;; Extra functionality for dired.
;; Open file with system utilities, hide files, etc.
(use-package dired-x
  :config
  (let ((cmd (cond (sys/mac "open")
                   (sys/linux "xdg-open")
                   (sys/win "start")
                   (t ""))))
    ;; Rules for suggested commands.
    ;; Take precedence over `dired-guess-shell-alist-default'.
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))


;;; dired-hacks

(celeste/prepare-package dired-hacks)

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<backtab>" . dired-subtree-remove)))

(use-package dired-narrow
  :after dired
  :commands (dired-narrow
             dired-narrow-regexp
             dired-narrow-fuzzy))


(provide 'init-dired)
;;; init-dired.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
