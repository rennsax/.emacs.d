;;; init-env.el -- Inherit envs from shell. -*- lexical-binding: t -*-
;;; Commentary:

;; Load environment variables from shell. Powered by purcell's
;; exec-path-from-shell, with some cache mechanism added.

;;; Code:

(setq celeste-inherit-shell-env-list
      '("PATH"
        "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_STATE_HOME"
        "https_proxy" "http_proxy"
        "GOPATH"))

(celeste/require 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (setq exec-path-from-shell-variables celeste-inherit-shell-env-list)

  (defun +getenv-shell (variable &optional frame always)
  "Get the environment variable VARIABLE from shell.

VARIABLE and FRAME is passed to `getenv'.

If ALWAYS is non-nil, always try to copy env from shell.
Otherwise, if `getenv' returns non-nil, the result is returned
immediately."
  (if always
      (cdar (exec-path-from-shell-copy-envs (list variable)))
    (or (getenv variable frame)
        (cdar (exec-path-from-shell-copy-envs (list variable))))))

  ;; Cache mechanism for `exec-path-from-shell'.
  (defvar exec-path-from-shell-cache-file (concat user-emacs-directory "shell-env"))
  (defvar exec-path-from-shell-no-cache nil)

  (defun +exec-path-from-shell-write-cache (env-list)
    "Write ENV-LIST to cache.

ENV-LIST is a list of (NAME . VALUE)."
    (with-temp-buffer
      (insert ";;; shell-env -- Cache file for exec-path-from-shell. -*- lexical-binding: t -*-
;;; Commentary:

;; DO NOT EDIT! Auto-generated by `+exec-path-from-shell-write-cache'.

;;; Code:

")
      (mapc (lambda (pair)
              (let ((name (car pair))
                    (value (cdr pair)))
                (insert (format "(setenv \"%s\" \"%s\")\n" name value))
                ;; Handle PATH specially.
                (when (string-equal "PATH" name)
                  (insert (format "(setq exec-path '(%s))\n"
                                  (string-join (mapcar (lambda (string-var) (format "\"%s\"" string-var))
                                                       (append (parse-colon-path value) (list exec-directory)))
                                               " ")))
                  (insert (format "(setq-default eshell-path-env \"%s\")\n" value)))))
            env-list)
      (insert ";;; shell-env ends here\n")
      (write-region nil nil exec-path-from-shell-cache-file)))

  (defun +exec-path-from-shell-invalidate-cache ()
    "Invalidate the cache for envs.

This must be called manually after the shell environment
variables are changed."
    (interactive)
    (delete-file exec-path-from-shell-cache-file))

  (advice-add 'exec-path-from-shell-initialize :around
              (defun +exec-path-from-shell-respect-cache-a (old-function)
                (if exec-path-from-shell-no-cache
                    (funcall old-function)
                  (let ((cache-file exec-path-from-shell-cache-file))
                    (if (file-exists-p exec-path-from-shell-cache-file)
                        (progn
                          (message "[exec-path-from-shell] read envs from cache")
                          (load exec-path-from-shell-cache-file))
                      (let ((env-list (funcall old-function)))
                        (unless exec-path-from-shell-no-cache
                          (+exec-path-from-shell-write-cache env-list))))))))

  ;; When GUI is enabled and the OS is not Windows.
  (when (and (display-graphic-p) (not sys/win))
    ;; This line increases 100-200 ms startup time, but it's in the critical
    ;; section :(. I currently enable a cache mechanism to reduce the side
    ;; effects. It's implemented by writing `setenv's to a cache file.
    (exec-path-from-shell-initialize)))

(provide 'init-env)
;;; init-env.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End: