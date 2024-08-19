;;; unfill.el -- Unfill commands for Emacs -*- lexical-binding: t -*-

;; Author: Bojun Ren <bj.ren.coding@outlook.com>
;; Package-Requires: ((emacs "29.3") (dash "2.13"))

;;; Commentary:

;; This package provides some useful functions to join multiple lines. The name
;; "unfill" derives from the package `fill', which usually divides a single line
;; according to `fill-column'.

;; `unfill-string' tries to handle CJK characters well.

;;; Code:

(require 'dash)

(defun unfill--do-join (contents left right &optional space)
  (replace-regexp-in-string
   (rx (group (regexp (eval left)))
       (* space) ?\n (* space)
       (group (regexp (eval right))))
   (if space "\\1 \\2" "\\1\\2") contents))

;;;###autoload
(defun unfill-string (contents)
  "Join CONTENTS that are separated by `fill-paragraph'."
  (let* (;; https://github.com/vinta/pangu.js/blob/6107055384b99e6f30a49f5d1b85aa0b78251dc2/src/shared/core.js#L19
         (cjk-charset '(in (?\u2e80 . ?\u2eff) (?\u2f00 . ?\u2fdf) (?\u3040 . ?\u309f) (?\u30a0 . ?\u30fa) (?\u30fc . ?\u30ff) (?\u3100 . ?\u312f) (?\u3200 . ?\u32ff) (?\u3400 . ?\u4dbf) (?\u4e00 . ?\u9fff) (?\uf900 . ?\ufaff)))
         (cjk-symbol-punctuation-charset '(in (?\u3000 . ?\u303f) (?\uff00 . ?\uffef)))
         ;; (others `(not (or ,cjk-charset ,cjk-symbol-punctuation-charset)))

         (cjk-charset-regexp (rx-to-string cjk-charset))
         (cjk-symbol-punctuation-charset-regexp (rx-to-string cjk-symbol-punctuation-charset))
         ;; (others-regexp (rx-to-string others))
         )

    (-> contents
        (unfill--do-join cjk-charset-regexp cjk-charset-regexp)
        (unfill--do-join cjk-charset-regexp "." t)
        (unfill--do-join cjk-symbol-punctuation-charset-regexp ".")
        (unfill--do-join "." "." t))))

;;;###autoload
(defun unfill-region (beg end)
  "Unfill the region betweeen BEG and END.

It can be used as a reverse operation for `fill-region'."
  (interactive "*r")
  (when (and transient-mark-mode
             (not mark-active))
    (user-error "No activated region!"))
  (replace-region-contents beg end (lambda () (unfill-string (buffer-string)))))

(provide 'unfill)
;;; unfill.el ends here
