;;; init-cjk.el -- Better CJK supports. -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.3") (dash "2.13") (unfill))

;;; Commentary:

;; This configuration offers better CJK editing experiences with some opinioned
;; tweaks.

;; Known limitations:

;;   - `fill-paragraph' is not tweaked. `fixup-whitespace' is hacked, but
;;     `join-line' is still problematic in some case. Specifically, when joining
;;     two lines like this:
;;
;;                                    |你好。|    <- Ended with a CJK symbol.
;;        Begin with an ASCII char -> |Hello.|
;;
;;     With `fill-paragraph' or `join-line', you probably will get
;;
;;                                    |你好。 Hello.|
;;                                          ^___ wrong space
;;
;;     The problem is that an extra space is inserted. This is incorrect. No
;;     space should be inserted between a CJK symbol (technically speaking,
;;     U+3000..U+303F && U+FF00..U+FFEF, namely, CJK Symbols and Punctuation &&
;;     Fullwidth ASCII variants) and an ASCII character.
;;
;;     `unfill' can deal with this case correctly, but I do not apply this kind
;;     of hack because this narrow use case.

;;; Code:


;; Allowing breaking after CJK characters and improves the word-wrapping for CJK
;; text mixed with Latin text.
(setq word-wrap-by-category t)

;; Fix: `join-line' adds an inappropriate space between two CJK characters.
(define-advice fixup-whitespace (:override () fix-cjk)
  "Like `fixup-whitespace', but also consider CJK characters."
  (interactive "*")
  (let ((right-regex (rx (| bol eol
                            (syntax close-parenthesis))))
        (left-regex (rx (| eol
                           (syntax open-parenthesis)
                           (syntax expression-prefix))))
        (cjk-regex (rx multibyte)))
    (save-excursion
      (delete-horizontal-space)
      (unless (or (looking-at right-regex)
              (save-excursion (forward-char -1)
                              (looking-at left-regex))
              ;; If characters on the left and right are both CJK, then do not
              ;; insert space.
              (and (looking-at cjk-regex)
                   (save-excursion (forward-char -1)
                                   (looking-at cjk-regex))))
        (insert ?\s)))))


;;; A series of routines to improve CJK editing experience in org-mode.

;; ZWSP is recommended to be used, see (info "(org) Escape Character").

(with-eval-after-load 'org
  (define-advice org-emphasize (:override (&optional char) may-add-zws)
    "Like `org-emphasize', but may add ZWS around the region according to the context."
    (interactive)
    (let ((erc org-emphasis-regexp-components)
          (string "")
          (cjk-regex (rx multibyte))
          (insert-zws (lambda () (insert ?\u200b)))
          beg end move s)
      (if (org-region-active-p)
          (setq beg (region-beginning)
                end (region-end)
                string (buffer-substring beg end))
        (setq move t))

      (unless char
        (message "Emphasis marker or tag: [%s]"
                 (mapconcat #'car org-emphasis-alist ""))
        (setq char (read-char-exclusive)))
      (if (equal char ?\s)
          (setq s ""
                move nil)
        (unless (assoc (char-to-string char) org-emphasis-alist)
          (user-error "No such emphasis marker: \"%c\"" char))
        (setq s (char-to-string char)))
      (while (and (> (length string) 1)
                  (equal (substring string 0 1) (substring string -1))
                  (assoc (substring string 0 1) org-emphasis-alist))
        (setq string (substring string 1 -1)))
      (setq string (concat s string s))
      (when beg (delete-region beg end))
      (unless (or (bolp)
                  (string-match (concat "[" (nth 0 erc) "\n]")
                                (char-to-string (char-before (point)))))
        ;; If the previous character is CJK, then insert ZWS.
        (if (save-excursion (forward-char -1) (looking-at cjk-regex))
            (funcall insert-zws)
          (insert " ")))
      (unless (or (eobp)
                  (string-match (concat "[" (nth 1 erc) "\n]")
                                (char-to-string (char-after (point)))))
        ;; If the next character is CJK, then insert ZWS.
        (if (looking-at cjk-regex)
            (funcall insert-zws)
          (insert " "))
        (backward-char 1))
      (insert string)
      (and move (backward-char 1))))

  ;; Fix `org-emphasize' with zero-width space (ZWS).
  ;;   NOTE: ZWS also acts as the escape character in org-mode.
  ;;   See `(info "(org) Escape Character")'.
  (add-hook 'org-mode-hook (defun +prettify-zwsp ()
                             "Set prettify symbol for zero-width space."
                             (setq-local prettify-symbols-alist
                                         '(("\u200b" . "\u02d4")))
                             (prettify-symbols-mode)))

  ;; Insert ZWSP easily with M-SPC (`cycle-spacing' before)
  (keymap-set org-mode-map "M-SPC" (lambda () (interactive) (insert "\u200b"))))

;; When exported, remove zero-width spaces.
(with-eval-after-load 'ox
  (defun +org-export-remove-zwsp (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string "\u200b" "" text)))
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-zwsp))


;; Fix: `fill-paragraph' sometimes breaks a whole Chinese sentence with newline,
;; and when the paragraph exported, the newline character is replaced with a
;; space, which is not the desired result.
(use-package unfill
  :init (celeste/prepare-package dash)
  :commands unfill-region
  :autoload unfill-string)

(defun ox--paragraph-join-lines-a (args)
  (let ((contents (nth 1 args)))
    (setf (cadr args) (unfill-string contents)) args))

(with-eval-after-load 'ox-html
  (advice-add 'org-html-paragraph :filter-args #'ox--paragraph-join-lines-a))

;; `org-hugo-paragraph' will invoke `org-md-paragraph'. Though it has its own logic
;; to handle CJK characters, I also add my own logic.
(with-eval-after-load 'ox-md
  (advice-add #'org-md-paragraph :filter-args #'ox--paragraph-join-lines-a))

;; This is independent with `ox-md'.
(with-eval-after-load 'ox-gfm
  (advice-add #'org-gfm-paragraph :filter-args #'ox--paragraph-join-lines-a))


(provide 'init-cjk)
;;; init-cjk.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
