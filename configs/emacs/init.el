;;; init -- Emacs init file

;;; Commentary:
;; Define the init file

;;; Code:
;; We're actually loading configuration from init.org.
(require 'org)
(org-babel-load-file
  (expand-file-name "settings.org"
                    user-emacs-directory))
