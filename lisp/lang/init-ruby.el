;;; init-ruby.el -- Ruby support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'ruby-mode
  (celeste/setup-lang ruby
    :modes (ruby-mode ruby-ts-mode)
    :flycheck ruby-rubocop
    :project-identify ("Gemfile")
    :add-hook t)
  )

(with-eval-after-load 'ruby-mode
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

(provide 'init-ruby)
;;; init-ruby.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
