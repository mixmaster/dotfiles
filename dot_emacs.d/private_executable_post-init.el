;;; post-init.el --- Post Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Environment
(defconst *is-macos* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'linux))
(defconst *is-unix* (or *is-macos* *is-linux*))
(defconst *is-windows* (eq system-type 'windows-nt))

;; Fonts
(defconst *mono-font-family*
  (if *is-unix* "ProFont IIx Nerd Font Mono" "JetBrainsMono Nerd Font"))
(defconst *mono-font-height*
  (if *is-unix* 130 130))
(defconst *serif-font-family*
  (if *is-unix* "IBM Plex Serif" "Georgia"))
(defconst *serif-font-height*
  (if *is-unix* 110 110))
(defconst *project-dir* (expand-file-name "~/Playground"))

;; Speed up Emacs by automatically byte-compiling and native compiling
;; all .el files
(use-package compile-angel
  :ensure t
  :demand t
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

(use-package better-defaults
  :straight (better-defaults :type git :host nil :repo "https://git.sr.ht/~technomancy/better-defaults")
  :demand t)

(setq default-directory "~/"
      ;; always follow symlinks when opening files
      vc-follow-symlinks t
      ;; overwrite text when selected, like we expect.
      delete-seleciton-mode t
      ;; quiet startup
      inhibit-startup-message t
      initial-scratch-message nil
      ;; hopefully all themes we install are safe
      custom-safe-themes t
      ;; simple lock/backup file management
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      ;; when quiting emacs, just kill processes
      confirm-kill-processes nil
      ;; ask if local variables are safe once.
      enable-local-variables t
      ;; life is too short to type yes or no
      use-short-answers t

      ;; clean up dired buffers
      dired-kill-when-opening-new-dired-buffer t)

;; use human-readable sizes in dired
(setq-default dired-listing-switches "-alh")

;; always highlight code
(global-font-lock-mode 1)

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs feature that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)

(winner-mode 1)
(delete-selection-mode 1) ; Replace selected text with typed text

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; Use human-readable sizes in dired
(setq-default dired-listing-switches "-alh")

;; No littering
(use-package no-littering
  :demand t
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; General.el
(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-create-definer leader-def
    :states '(normal motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (leader-def
    "" '(:ignore t :wk "leader")

    "b" '(:ignore t :wk "buffer")
    "bb" 'consult-buffer
    "bd" 'kill-this-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bx" 'kill-buffer-and-window

    "f" '(:ignore t :wk "file")
    "ff" 'ido-find-file
    "fr" 'consult-recent-file
    "fs" 'save-buffer

    "w" '(:ignore t :wk "window")
    "wd" 'delete-window
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wm" 'delete-other-window
    "ws" 'split-window-below
    "wv" 'split-window-right
    "ww" 'other-window

    "q" '(:ignore t :wk "quit")
    "qq" 'save-buffers-kill-terminal
    "qr" 'restart-emacs

    "c" '(:ignore t :wk "checks")
    "t" '(:ignore t :wk "toggle")
    "s" '(:ignore t :wk "straight")
    "sf" 'straight-x-fetch-all
    "sp" 'straight-x-pull-all
    "sr" 'straight-remove-unused-repos
    "ss" 'straight-get-recipe)

  (general-create-definer localleader-def
    :states '(normal motion emacs)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")
  (localleader-def "" '(:ignore t :wk "mode")))

;; Vim emulation
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-respect-visual-line-mode t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure t
  :after evil
  :defer t
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

(use-package evil-escape
  :ensure t
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.1)
  :config
  (evil-escape-mode 1))

(use-package evil-lion
  :commands evil-lion-left evil-lion-right
  :after evil general
  :general
  (:states 'normal
           "ga"  'evil-lion-left
           "gA"  'evil-lion-right)
  (:states 'visual
           "ga"  'evil-lion-left
           "gA"  'evil-lion-right))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :hook (after-init . vim-tab-bar-mode))

(use-package vdiff
  :ensure t
  :defer t
  :commands (vdiff-buffers
             vdiff-buffers3
             vdiff-quit
             vdiff-files
             vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))

                                        ; vterm
(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))

;; Vertico + Consult + Embark
(use-package vertico
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverage Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginaalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               `("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definition-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; Code completion with Corfu
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*"))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) ;; Disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1)
  (corfu-echo-mode 1)
  (corfu-popupinfo-mode 1))

(use-package kind-all-the-icons
  :straight
  (kind-all-the-icons :type git :host github
                      :repo "Hirozy/kind-all-the-icons")
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

;; LSP
(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-format-buffer)
  :custom
  (eglot-report-progress nil) ; Prevent minibuffer spam

  :config
  ;; Optimization
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil))

;; Hide warnings and display only errors
(setq warning-minimum-level :error)

(use-package which-key
  :ensure t
  :after evil
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer))

;; Flycheck
(use-package flycheck
  :defer 1
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :init
  (global-flycheck-mode t))

(use-package flycheck-posframe
  :defer 1
  :after flycheck
  :hook (flycheck-moe . flycheck-posframe-mode)
  :config
                                        ; (setq flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-posframe-configure-pretty-defaults)
                                        ; (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
  (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p))

(use-package flycheck-eglot
  :defer 1
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package parinfer-rust-mode
  :defer 1
  :hook
  emacs-lisp-mode
  lisp-mode
  clojure-mode
  :custom
  (parinfer-rust-auto-download t))

(use-package helpful
  :defer 1
  :general
  (leader-def
    "h" '(:ignore t :wk "help")
    "hf" 'helpful-function
    "hv" 'helpful-variable
    "hk" 'helpful-key
    "ho" 'helpful-at-point
    "hs" 'helpful-symbol)
  :config
  (add-to-list 'display-buffer-alist
               '("*[Hh]elp"
                 (display-buffer-reuse-mode-window
                  display-buffer-pop-up-window))))

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :defer 1
  :config
  (apheleia-global-mode +1))

(use-package magit
  :commands magit
  :general
  (leader-def
    "g"  '(:ignore t :wk "git")
    "gs" '(magit :wk "git status")
    "gg" '(magit :wk "git status"))
  :custom
  (magit-repository-directories `((,*project-dir* . 3)))
  :config
  ;; speed up magit for large repos
  (dir-locals-set-class-variables
   'huge-git-repository
   '((magit-status-mode
      . ((eval . (magit-disable-section-inserter 'magit-insert-tags-header)))))))

(use-package magit-todos
  :after magit
  :commands magit-todos-list magit-todos-mode
  :general
  (leader-def
    "gt" 'magit-todos-list))

(use-package magit-delta
  :after magit
  :commands magit-delta-mode
  :hook (magit-mode . magit-delta-mode))

;; Treemacs
(use-package treemacs
  :defer 1
  :commands treemacs treemacs-find-file
  :general
  (leader-def
    "tt" 'treemacs
    "tf" 'treemacs-find-file))

(use-package treemacs-evil
  :defer 1
  :after treemacs evil)

(use-package treemacs-projectile
  :defer 1
  :after treemacs projectile)

(use-package treemacs-magit
  :defer 1
  :after treemacs magit)

;; Projectile
(defun me/expand-git-project-dirs (root)
  "Return a list of all project directories 2 levels deep in ROOT.

  Given my git projects directory ROOT, with a layout like =git/{hub,lab}/<user>/project=, return a list of 'user' directories that are part of the ROOT."
  (mapcan #'(lambda (d) (cddr (directory-files d t)))
          (cddr (directory-files root t))))

(use-package projectile
  :defer 1
  :general
  (leader-def
    "p" '(:ignore t :wk "project")
    "pP" 'projectile-switch-project
    "pd" 'projectile-dired
    "pb" 'projectile-switch-to-buffer
    "pf" 'projectile-find-file
    "p/" 'projectile-ripgrep)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-sort-order 'recently-active)
  (projectile-indexing-method 'hybrid)
  (projectile-project-search-path `((,*project-dir* . 3)))
  :config
  (projectile-save-known-projects)
  (projectile-mode +1))

;; Look & feel
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)

(use-package unicode-fonts
  :demand t
  :config
  (unicode-fonts-setup)
  (custom-set-faces
   `(default ((t (:family ,*mono-font-family*
                          :height ,*mono-font-height*))))
   `(variable-pitch ((t (:family ,*serif-font-family*
                                 :height ,*serif-font-height*))))))

(use-package one-themes
  :ensure t
  :config
  (load-theme 'one-dark t))

(use-package minions
  :defer 1
  :config
  (minions-mode 1))

(use-package doom-modeline
  :ensure t
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-hud t)
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :defer 1
  :after all-the-icons
  :hook (dired-mode . all-the-icon-dired-mode))

(use-package treemacs-all-the-icons
  :defer 1
  :after all-the-icons treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-completion
  :defer 1
  :after all-the-icons
  :config
  (add-hook 'marginalia-mode-hook
            #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode 1))

(defun me/hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(use-package whitespace-cleanup-mode
  :demand t
  :hook
  (special-mode     . me/hide-trailing-whitespace)
  (comint-mode      . me/hide-trailing-whitespace)
  (compilation-mode . me/hide-trailing-whitespace)
  (term-mode        . me/hide-trailing-whitespace)
  (vterm-mode       . me/hide-trailing-whitespace)
  (shell-mode       . me/hide-trailing-whitespace)
  (minibuffer-setup . me/hide-trailing-whitespace)
  :custom
  (show-trailing-whitespace t)
  :config
  (global-whitespace-cleanup-mode 1))

(use-package rainbow-delimiters
  :defer 1
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "red"
                      :inherit 'error
                      :box t))(setq default-directory "~/")
;; always follow symlinks when opening files
vc-follow-symlinks t
;; overwrite text when selected, like we expect.
delete-seleciton-mode t
;; quiet startup
inhibit-startup-message t
initial-scratch-message nil
;; hopefully all themes we install are safe
custom-safe-themes t
;; simple lock/backup file management
create-lockfiles nil
backup-by-copying t
delete-old-versions t
;; when quiting emacs, just kill processes
confirm-kill-processes nil
;; ask if local variables are safe once.
enable-local-variables t
;; life is too short to type yes or no
use-short-answers t

;; clean up dired buffers
dired-kill-when-opening-new-dired-buffer t

;; use human-readable sizes in dired
(setq-default dired-listing-switches "-alh")

;; always highlight code
(global-font-lock-mode 1)
;; refresh a buffer if changed on disk
(global-auto-revert-mode 1)
