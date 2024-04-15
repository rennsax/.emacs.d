;;; -*- lexical-binding: t; -*-

;; From doom.

;;;###autoload
(defun doom-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
              -1)
          (string-trim (buffer-string)))))

;;;###autoload
(defface +eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
  "TODO"
  :group 'eshell)

;;;###autoload
(defface +eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
  "TODO"
  :group 'eshell)

(defun +eshell--current-git-branch ()
  ;; TODO Refactor me
  (cl-destructuring-bind (status . output)
      (doom-call-process "git" "symbolic-ref" "-q" "--short" "HEAD")
    (if (equal status 0)
        (format " [%s]" output)
      (cl-destructuring-bind (status . output)
          (doom-call-process "git" "describe" "--all" "--always" "HEAD")
        (if (equal status 0)
            (format " [%s]" output)
          "")))))

(setq eshell-prompt-regexp "^[^#$\n]* [#❯] ")
;;;###autoload
(defun +eshell-default-prompt-fn ()
  "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
  (require 'shrink-path)
  (concat (if (bobp) "" "\n")
          (let ((pwd (eshell/pwd)))
            (propertize (if (equal pwd "~")
                            pwd
                          (abbreviate-file-name (shrink-path-file pwd)))
                        'face '+eshell-prompt-pwd))
          (propertize (+eshell--current-git-branch)
                      'face '+eshell-prompt-git-branch)
          (propertize " ❯" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))
