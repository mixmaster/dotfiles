;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Donald Luo"
      user-mail-address "donaldluo@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 15)
  doom-big-font (font-spec :family "Hack Nerd Font Mono" :size 18)
  doom-variable-pitch-font (font-spec :family "Overpass Nerd Font" :size 15)
  doom-serif-font (font-spec :family "BlexMono Nerd Font" :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; I prefer esc sequence to be "fd" instead of "jk".
(setq-default evil-escape-key-sequence "fd")

;; No annoying confirmation message at exit.
(setq-default confirm-kill-emacs nil)

;; Better defaults
(setq-default
  delete-by-moving-to-trash t
  uniquify-buffer-name-style 'forward
  window-combination-resize t
  x-stretch-cursor t)

(setq undo-limit 80000000
  evil-want-fine-undo t
  auto-save-default t
  inhibit-compacting-font-caches t
  truncate-string-ellipsis "...")

(delete-selection-mode 1)
(display-time-mode 1)
(unless (equal "Battery status not available"
          (battery))
  (display-battery-mode 1))
(global-subword-mode 1)

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq evil-vsplit-window-right t
  evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

(map! :map evil-window-map
  "SPC" #'rotate-layout)

;; Visual settings
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case."
  (setq-local doom-modeline-buffer-encoding
    (unless (or (eq buffer-file-coding-system 'utf-8-unix)
              (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Persist initial frame position and dimensions
(when-let (dims (doom-store-get 'last-frame-size))
  (cl-destructuring-bind ((left . top) width height fullscreen) dims
    (setq initial-frame-alist
      (append initial-frame-alist
        `((left . ,left)
          (top . ,top)
          (width . ,width)
          (height . ,height)
          (fullscreen . ,fullscreen))))))

(defun save-frame-dimensions ()
  (doom-store-put 'last-frame-size
    (list (frame-position)
      (frame-width)
      (frame-height)
      (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook #'save-frame-dimensions)
