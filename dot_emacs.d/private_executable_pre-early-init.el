;;; pre-early-init.el --- Pre Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

(setq custom-file null-device)
(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)

;; Improve I/O
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; Straight
(setq straight-use-package-by-default t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(find-when-checking)
      package-enable-at-startup nil
      vc-follow-symlinks t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight-x)

(straight-use-package 'use-package)

(use-package esup
  :demand t
  :commands esup)

(use-package benchmark-init
  :demand t
  :straight (:host github :repo "kekeimiku/benchmark-init-el")
  :hook (after-init . benchmark-init/deactivate))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format
             "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time)))
            gcs-done)))

(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))
