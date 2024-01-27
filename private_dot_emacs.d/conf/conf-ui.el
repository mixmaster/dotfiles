;;; package --- Summary

;;; Commentary:

;;; Code:

;; Font Configuration
(defvar efs/default-font-size 130)
(defvar efs/default-variable-font-size 130)
(set-face-attribute 'default nil :font "ProFont IIx Nerd Font" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "ProFont IIx Nerd Font" :height efs/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Overpass Nerd Font" :height efs/default-variable-font-size :weight 'regular)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)

  (if (display-graphic-p)
      (doom-themes-neotree-config))
  (doom-themes-org-config)

  :init
  (load-theme 'doom-one t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'conf-ui)

;;; conf-ui.el ends here
