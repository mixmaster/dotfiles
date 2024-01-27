;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (clojure-mode . lsp-deferred)
	 (clojurescript-mode . lsp-deferred)
	 (clojurec-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-diferred))

(use-package lsp-ui :commands lisp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(provide 'conf-lsp)

;;; conf-lsp.el ends here
