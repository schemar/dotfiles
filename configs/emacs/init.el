;;; init -- Emacs init file

;;; Commentary:
;; Define the init file

;;; Code:
;; Increase the garbage collection threshold in the hopes of improving perfomance.
;; The default is 800kB.
(setq gc-cons-threshold 100000000) ;; That is 100mB.

;; Create another auto-generated init file so that the handcrafted one won't be overwritten by Emacs.
    (setq custom-file (expand-file-name "auto-generated.el" user-emacs-directory))
    (when (file-exists-p custom-file)
      (load custom-file))

;; *Make sure to not use ~:ensure~ in any ~use-package~ blocks.*
     (setq package-enable-at-startup nil)
     (defvar bootstrap-version)
     (let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
       (unless (file-exists-p bootstrap-file)
         (with-current-buffer
           (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
           (goto-char (point-max))
           (eval-print-last-sexp)))
       (load bootstrap-file nil 'nomessage))
     (straight-use-package 'use-package)
     (setq straight-use-package-by-default t)

     (use-package org
       :init
       (require 'org))
;; We're actually loading configuration from settings.org.
;; This has to be done after the org package has been installed in order to load
;; the latest version instead of the built-in one.
(require 'org)
(org-babel-load-file
  (expand-file-name "settings.org"
                    user-emacs-directory))
