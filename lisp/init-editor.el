;;; init-editor.el -- Editor feature enhancement. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Quickly navigation.

;; Move what I mean!
(celeste/use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;; Rather convenient package that can serve as a complete alternative of Vim's
;; textobject!
(celeste/use-package expand-region
  :bind (("C-=" . er/expand-region)))

(celeste/use-package avy
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
  )


;;; Modal editing.

;; Celeste Emacs has provided presets for several modal editing packages.
(pcase celeste-modal-editing
  ('meow (require 'init-meow))
  ('evil
   (require 'init-evil)
   (require 'init-evil-keybinding)))


;;; Misc.

;; `anzu.el' provides a minor mode which displays 'current match/total
(celeste/use-package anzu
  :diminish
  :hook (after-init . global-anzu-mode))

;; rotate.el: provide convenient window layout management functions.
(celeste/package-autoload 'rotate)



(provide 'init-editor)
;;; init-editor.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
