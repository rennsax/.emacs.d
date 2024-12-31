;;; init-lang.el -- Language specified tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; IDE tools.

(require 'init-xref)
(require 'init-eglot)

(require 'init-flycheck)
(require 'init-format)
(require 'init-snippet)
(require 'init-treesit)
(require 'init-docs)
(require 'init-ctags)

(defun flycheck-eglot--enable-and-remove-self-h ()
  "This hook should be added to `eglot-managed-mode-hook'."
  (flycheck-eglot-mode +1)
  (remove-hook 'eglot-managed-mode-hook
               'flycheck-eglot--enable-and-remove-self-h))

(autoload 'eglot-managed-p "eglot")
(cl-defmacro celeste/setup-lang (lang &key modes eglot-server eglot flycheck add-hook project-identify)
  "Setup development environment for a specified language.

LANG: the language name.

MODES: the target major mode.

EGLOT-SERVER: add to `eglot-server-programs'.

Buffer-local options:

EGLOT: whether to enable eglot immediately.

FLYCHECK: the checker to be set to `flycheck-checker', should return t on
predicate `flycheck-valid-checker-p'. If `eglot' is given, use the
`flycheck-eglot' backend. If `default' or t is given, do not set
`flycheck-checker' but enable `flycheck-mode'.

ADD-HOOK: whether to add the setup function to the major mode hook.

PROJECT-IDENTIFY: add to `project-x-local-identifier'."

  (declare (indent defun))
  (when (seq-empty-p modes) (user-error "At least one mode should be specified!"))
  (let* ((setup-fun-name (format "celeste/%s-setup" lang))
         (setup-fun (intern setup-fun-name))
         result setup-sexp)
    (when eglot-server
      (cl-pushnew `(with-eval-after-load 'eglot
                     (add-to-list 'eglot-server-programs
                                  '(,modes . ,eglot-server)))
                  result))
    (when eglot (cl-pushnew '(eglot-ensure) setup-sexp))
    (when flycheck
      (require 'flycheck)
      (cond ((eq flycheck 'eglot)
             (cl-pushnew
              (quote
               ;; HACK: If the current buffer is not managed by elgot,
               ;; `flycheck-eglot--setup' does noting. And since `eglot-ensure'
               ;; is asynchronous, simply `(flycheck-elgot-mode +1)' probably
               ;; fail to setup `flycheck-elgot-mode'. If elgot isn't managing
               ;; this buffer currently, a hook is injected into
               ;; `eglot-managed-mode-hook' to enable `flycheck-eglot-mode'
               ;; later. The hook will remove itself, so it does not interfere
               ;; other settings.
               (if (eglot-managed-p)
                   (flycheck-eglot-mode +1)
                 (add-hook 'eglot-managed-mode-hook
                           (defun flycheck-eglot--enable-and-remove-self-h ()
                             (flycheck-eglot-mode +1)
                             (remove-hook 'eglot-managed-mode-hook
                                          'flycheck-eglot--enable-and-remove-self-h 'local))
                           nil 'local)))
              setup-sexp))
            ((or (eq flycheck 'default)
                 (eq flycheck t))
             (cl-pushnew '(flycheck-mode +1) setup-sexp))
            (t (unless (flycheck-valid-checker-p flycheck)
                 (warn "`%s' is not a valid flycheck checker!" flycheck))
               (cl-pushnew `(progn (setq-local flycheck-checker ',flycheck)
                                   (flycheck-mode +1))
                           setup-sexp))))
    (when add-hook
      (mapc (lambda (mode)
              (cl-pushnew `(add-hook ',(intern (format "%s-hook" mode)) ',setup-fun)
                          result))
            modes))
    (when project-identify
      (when (stringp project-identify) (setq project-identify (list project-identify)))
      (cl-pushnew `(with-eval-after-load 'project-x
                     (setq project-x-local-identifier
                           (append project-x-local-identifier
                                   ',project-identify)))
                  result))
    (unless (seq-empty-p setup-sexp)
      (cl-pushnew `(defun ,setup-fun ()
                     ,(format "Setup for %s." lang)
                     ,@setup-sexp)
                  result))
    (macroexp-progn result)))



;;; Programming Languages.

;; Markup.
(require 'init-yaml)
(require 'init-markdown)
(require 'init-json)

;; General-purpose.
(require 'init-go)
(require 'init-lua)
(require 'init-cc)
(require 'init-python)
(require 'init-rust)
(require 'init-ruby)

;; DSL.
(require 'init-sh)
(require 'init-elisp)
(require 'init-nix)

;; Misc.
(require 'init-misc-dsl)
(require 'init-web)
(require 'init-beancount)



(provide 'init-lang)
;;; init-lang.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
