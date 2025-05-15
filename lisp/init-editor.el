;;; init-editor.el -- Editor feature enhancement. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Quickly navigation.

;; Move what I mean!
(use-package mwim
  :init
  (celeste/prepare-package mwim)
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;; Rather convenient package that can serve as a complete alternative of Vim's
;; textobject!
(use-package expand-region
  :init
  (celeste/prepare-package expand-region)
  :bind (("C-=" . er/expand-region)))

(use-package avy
  :init
  (celeste/prepare-package avy)
  :bind (("C-'" . avy-goto-char-timer)
         ("C-:" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("C-c C-j" . avy-resume)
         ("M-g l" . avy-goto-line)
         :map isearch-mode-map
         ("C-'" . avy-isearch))
  :config
  ;; Ease the stress of my left hand.
  (setq avy-keys '(?h ?j ?k ?l ?g)
        ;; Also consider de-bruijn. Benefits of De Bruijn Sequence: a) the path
        ;; length is consistent in each avy session, b) it's interesting.
        avy-style 'words
        avy-background t)
  ;; Do not timeout. Manual RET triggers the selection of candidates.
  (setq avy-timeout-seconds most-positive-fixnum)

  ;; I removed some words that are difficult to type (like "if").
  (setq avy-words
        '("am" "if" "is" "it" "ox" "em" "sm"
          "ace" "act" "add" "age" "ago" "aim" "air" "ale" "all" "and" "ant" "any"
          "ape" "apt" "arc" "are" "arm" "art" "ash" "ate" "awe" "axe" "bad" "bag"
          "ban" "bar" "bat" "bay" "bed" "bee" "beg" "bet" "bid" "big" "bit" "bob"
          "bot" "bow" "box" "boy" "but" "cab" "can" "cap" "car" "cat" "cog" "cop"
          "cow" "cry" "cup" "cut" "day" "dew" "did" "die" "dig" "dim" "dip" "dog"
          "dot" "dry" "dub" "dug" "dye" "ear" "eat" "eel" "egg" "ego" "elf" "eve"
          "eye" "fan" "far" "fat" "fax" "fee" "few" "fin" "fit" "fix" "flu" "fly"
          "foe" "fog" "for" "fox" "fry" "fun" "fur" "gag" "gap" "gas" "gel" "gem"
          "get" "gig" "gin" "gnu" "god" "got" "gum" "gun" "gut" "guy" "gym" "had"
          "hag" "ham" "has" "hat" "her" "hid" "him" "hip" "his" "hit" "hop" "hot"
          "how" "hub" "hue" "hug" "hut" "ice" "icy" "imp" "ink" "inn" "ion" "ire"
          "ivy" "jab" "jam" "jar" "jaw" "jet" "job" "jog" "joy" "key" "kid" "kit"
          "lag" "lap" "lay" "let" "lid" "lie" "lip" "lit" "lob" "log" "lot" "low"
          "mad" "man" "map" "mat" "may" "men" "met" "mix" "mob" "mop" "mud" "mug"
          "nag" "nap" "new" "nil" "nod" "nor" "not" "now" "nun" "oak" "odd" "off"
          "oil" "old" "one" "orb" "ore" "ork" "our" "out" "owl" "own" "pad" "pan"
          "par" "pat" "paw" "pay" "pea" "pen" "pet" "pig" "pin" "pit" "pod" "pot"
          "pry" "pub" "pun" "put" "rag" "ram" "ran" "rat" "raw" "ray" "red" "rib"
          "rim" "rip" "rob" "rod" "rot" "row" "rub" "rug" "rum" "run" "sad" "sat"
          "saw" "say" "sea" "see" "sew" "she" "shy" "sin" "sip" "sit" "six" "ski"
          "sky" "sly" "sob" "son" "soy" "spy" "sum" "sun" "tab" "tad" "tag" "tan"
          "tap" "tar" "tax" "tea" "the" "tie" "tin" "tip" "toe" "ton" "too" "top"
          "toy" "try" "tub" "two" "urn" "use" "van" "war" "was" "wax" "way" "web"
          "wed" "wet" "who" "why" "wig" "win" "wit" "woe" "won" "wry" "you" "zap"
          "zip" "zoo"))

  ;; Read https://karthinks.com/software/emacs-window-management-almanac/#windows-are-made-up-let-s-ignore-them
  ;; Jump to the previous position after `avy-jump', w/o ruining the window configuration.
  (define-advice pop-global-mark (:around (pgm) use-display-buffer)
    "Make `pop-to-buffer' jump buffers via `display-buffer'."
    (cl-letf (((symbol-function 'switch-to-buffer)
               #'pop-to-buffer))
      (funcall pgm)))
  )


;;; Misc.

;; Smartly cleanup whitespaces on saving buffers. The package's idea is simple
;; but practical: if the file's whitespaces is *initially* clean (when visited),
;; then it will set `whitespace-cleanup-mode-only-if-initially-clean' to t, and
;; automatically cleanup whitespace. If the file is not initially clean,
;; manually `delete-trailing-whitespace' and then revert the buffer.
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init
  (celeste/prepare-package whitespace-cleanup-mode)
  :hook (after-init . global-whitespace-cleanup-mode)
  :config
  (setq whitespace-cleanup-mode-preserve-point t)
  ;; Prohibit `editorconfig' from adding `delete-trailing-whitespace' to
  ;; `write-file-functions'.
  (with-eval-after-load 'editorconfig
    (advice-add #'editorconfig--get-trailing-ws :override #'ignore)))

;; Simpler auto-save: just what you mean auto-save!
(use-package wim-auto-save
  :init
  ;; Disable Emacs's `auto-save-mode'. To check whether this minor mode is
  ;; enabled in the current buffer, evaluate:
  ;; (and buffer-auto-save-file-name (>= buffer-saved-size 0))
  (setq auto-save-default nil)

  (setq wim-auto-save-interval 1.5
        wim-auto-save-log-level 'basic)
  :hook (after-init . wim-auto-save-mode)
  :diminish
  :config
  (with-eval-after-load 'vundo
    (add-hook 'vundo-pre-enter-hook (lambda () (wim-auto-save-mode -1)))
    (add-hook 'vundo-post-exit-hook (lambda () (wim-auto-save-mode +1))))

  (setopt wim-auto-save-inhibit-actions '((wakatime-save . 5)
                                          (flycheck-handle-save . 5)))

  (setq wim-auto-save--debug nil)
  (setq wim-auto-save-log-level nil)

  (setq wim-auto-save-disable-predicates
        ;; If the current buffer is captured (`org-capture')
        (list (lambda () (get-buffer (concat "CAPTURE-" (buffer-name))))
              ;; `org-edit-src-code' uses an overlay to protect the source block
              ;; from being modified when the dedicated buffer is opened. The
              ;; name of the dedicated buffer is generated by
              ;; `org-src--construct-edit-buffer-name' by default.
              (lambda () (and
                     (derived-mode-p 'org-mode)
                     (seq-some
                      (lambda (buf) (string-match-p
                                (rx-to-string `(seq "*Org Src " ,(buffer-name) "[ " (* nonl) " ]") t)
                                (buffer-name buf)))
                      (buffer-list))))
              (lambda () (and
                     (derived-mode-p 'markdown-mode)
                     (seq-some
                      (lambda (buf) (string-match-p
                                (rx-to-string `(seq "*edit-indirect " ,(buffer-name) "*") t)
                                (buffer-name buf)))
                      (buffer-list))))
              (lambda () (bound-and-true-p tempel--active))))
  )

;; Undo keyboard macros in a single step
(require 'block-undo)


;; Edit the current buffer as super user with `sudo-edit'.
(use-package sudo-edit
  :init (celeste/prepare-package sudo-edit)
  :commands (sudo-edit sudo-edit-find-file)
  :config
  (sudo-edit-indicator-mode +1))


(use-package lorem-ipsum
  :init (celeste/prepare-package lorem-ipsum)
  :commands (lorem-ipsum-insert-list
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-paragraphs)
  :config
  (setq-default lorem-ipsum-list-bullet "- "
                lorem-ipsum-sentence-separator (if sentence-end-double-space "  " " "))

  (with-eval-after-load 'latex
    (add-to-list 'LaTeX-mode-hook
                 (lambda ()
                   (setq lorem-ipsum-list-bullet "\\item "))))
  )


(provide 'init-editor)
;;; init-editor.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
