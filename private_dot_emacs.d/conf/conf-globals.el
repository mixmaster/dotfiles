;;; package --- Summary

;;; Commentary:

;;; Code:

(scroll-bar-mode -1)            ; Disable visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar
(tooltip-mode -1)               ; Disable tooltips
(set-fringe-mode 10)            ; Give some breathing room
(menu-bar-mode -1)              ; Disable the menu bar
(desktop-save-mode 1)           ; Save sessions

(setq make-backup-files nil)    ; Disable creating backup~ files
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Line and column numbers

(column-number-mode)
(global-display-line-numbers-mode t)
    
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq make-backup-files nil)    ; Disable creating backup~ files
(setq custom-file (concat user-emacs-directory "/custom.el"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Make Esc quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; For macOS only
(when (eq system-type 'darwin)
  (if (eq mac-command-modifier 'meta)
      (progn
	(setq mac-command-modifier 'super)
	(setq mac-option-modifier 'meta)
	(message "Command is bound to SUPER and Optio is bound to META."))))

(provide 'conf-globals)

;;; conf-globals.el ends here
