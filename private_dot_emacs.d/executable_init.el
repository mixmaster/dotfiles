;;; package --- Summary

;;; Commentary:

;;; Code:

(setq inhibit-startup-message t)

(defvar conf-dir (expand-file-name "conf" user-emacs-directory)
  "The dir for individual confs.")
(add-to-list 'load-path conf-dir)

;; Set up package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Individual configurations
(require 'conf-globals)
(require 'conf-ui)
(require 'conf-neotree)
(require 'conf-completion)
(require 'conf-projectile)
(require 'conf-prog)
(require 'conf-clojure)
(require 'conf-smartparens)

;;; init.el ends here
