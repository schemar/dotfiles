;;; init -- Emacs init file

;;; Commentary:
;; Define the init file

;;; Code:
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
