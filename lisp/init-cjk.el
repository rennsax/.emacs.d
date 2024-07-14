;;; init-cjk.el -- Better CJK supports. -*- lexical-binding: t; -*-
;;; Commentary:
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


;; Smart input source.
(use-package sis
  :bind (("s-I" . sis-switch))          ; switch IME faster than macOS
  :commands (sis-global-context-mode
             sis-global-respect-mode
             sis-global-cursor-color-mode
             sis-set-english)
  :init
  (celeste/prepare-package sis)

  (defun celeste/sis--on ()
    "Turn on sis, smart source input."
    ;; Auto-switch IME according to the characters in the context.
    (sis-global-context-mode +1)
    ;; Use different cursor colors for different IME.
    (sis-global-cursor-color-mode +1)
    ;; Respect C-x, C-h, C-c, and so on.
    (sis-global-respect-mode +1))

  (defun celeste/sis--off ()
    "Turn off sis, smart source input."
    (sis-global-context-mode -1)
    (sis-global-cursor-color-mode -1)
    (sis-global-respect-mode -1))

  (define-minor-mode celeste/sis-mode
    "Toggle sis mode."
    :init-value nil
    :lighter " SIS"
    :global t
    (if celeste/sis-mode
        (celeste/sis--on)
      (celeste/sis--off)))

  :config
  (add-to-list 'sis-prefix-override-keys "M-g")
  ;; Make sure your input sources are these two (hint: use macism)
  (setq sis-english-source "com.apple.keylayout.US"
        sis-other-source "com.sogou.inputmethod.sogou.pinyin"))


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

;; Fix: `fill-paragraph' sometimes breaks a whole Chinese sentence. When
;; exported as HTML, a space kept at the line break.
;; https://emacs-china.org/t/org-mode-html/7174/2
(defun ox-join-cjk-lines-a (args)
    "Join consecutive Chinese lines.into a single long line without
unwanted space when exporting org-mode."
  (let* ((origin-contents (nth 1 args))
           (fix-regexp "[[:multibyte:]]") ; REVIEW: is it good enough to match CJK chars?
                                          ; See https://emacs-china.org/t/join-line/10355/8.
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
             "\\1\\2" origin-contents)))
      (setf (cadr args) fixed-contents) args))

(with-eval-after-load 'ox-html
  (advice-add 'org-html-paragraph :filter-args #'ox-join-cjk-lines-a))

;; Add advice to `org-md-paragraph' is also reasonable. But I want to keep the
;; markdown backend untouched.
(with-eval-after-load 'ox-hugo
  (advice-add 'org-hugo-paragraph :filter-args #'ox-join-cjk-lines-a))

(with-eval-after-load 'ox-latex
  ;; Include xeCJK package so Chinese can be correctly exported.
  (add-to-list 'org-latex-packages-alist '("" "xeCJK")))



(provide 'init-cjk)
;;; init-cjk.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
