;;; early-init.el -- Early init procedure. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Customization 🌷
(require 'init-custom)

;; PERF: if `file-name-handler-alist' is non-nil, Emacs use regex to match each
;; file for upcomming I/O operators. During the startup process, we eliminate
;; the feature. The value is put into another place so that we can restore it
;; later.
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; PERF: a common hack on GC. We don't need frequent GC during startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6) ;; TODO: what's the meaning of, "If this portion
                              ;; is smaller than ‘gc-cons-threshold’, this is
                              ;; ignored"?
;; However, after startup, it is important to set these to reasonable defaults.
(add-hook 'emacs-startup-hook
  (lambda () (setq gc-cons-threshold 16777216 ; (* 16 1024 1024)
        gc-cons-percentage 0.1
        file-name-handler-alist last-file-name-handler-alist)))

;; PERF: Emacs calls `package-activate-all' right after this file is loaded. Set
;; this value to nil to prevent that, since I just do not use a package manager!
(setq package-enable-at-startup nil)

;; Set to `t' means the frame size never changes
;; implicitly when there’s no window system support.
;; Namely, inhibit resizing frame.
(setq frame-inhibit-implied-resize t)

;;; From doom-start.el
;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters can
;;   trigger/queue a superfluous (and expensive, depending on the window system)
;;   frame redraw at startup.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Set alpha of the frame.
(push '(alpha . 98) default-frame-alist)


;;; Font settings.
;; Useful hooks:
;; `after-init-hook': not helpful in daemon mode.
;; `server-after-make-frame-hook': triggered when the daemon create a new frame.
;; `text-scale-mode-hook': TODO
;; `after-setting-font-hook': after the frame font is *changed*.

(create-fontset-from-fontset-spec
 (format  "-*-%s-regular-normal-normal-*-%d-*-*-*-p-0-fontset-celeste"
          celeste-default-font-name celeste-font-size))

;; This workaround is found at https://emacs-china.org/t/doom-emacs/23513
;; See the variable `char-script-table'.
(defun +fontset-setup-cjk (&optional fontset)
  "Setup special CJK fonts for FONTSET."
  (dolist (charset '(kana han cjk-misc bopomofo symbol))
    (set-fontset-font fontset charset (font-spec :family celeste-cjk-font-name))))

(add-hook 'after-setting-font-hook #'+fontset-setup-cjk)

;; `after-setting-font-hook' isn't triggered since the font of the initial frame
;; is never *changed*.
(+fontset-setup-cjk "fontset-celeste")

(push '(font . "fontset-celeste") default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
