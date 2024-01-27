;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package neotree
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
    (setq neo-window-fixed-size nil)
    (setq neo-theme 'nerd2)
    (setq neo-window-width 35)
    (global-set-key [f2] 'neotree-toggle)
    (global-set-key [f8] 'neotree-dir)))

(provide 'conf-neotree)

;;; conf-neotree.el ends here
