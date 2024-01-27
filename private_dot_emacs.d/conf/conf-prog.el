;;; package --- Summary

;;; Commentary:

;;; Code:

;; Magit Configuration
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Syntax check
(use-package flycheck
  :init (global-flycheck-mode))

(provide 'conf-prog)

;;; conf-prog.el ends here
