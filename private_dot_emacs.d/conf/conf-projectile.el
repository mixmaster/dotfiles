;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)))

(provide 'conf-projectile)

;;; conf-projectile.el ends here
