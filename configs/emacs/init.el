;;; init -- Emacs init file

;;; Commentary:
;; Define the init file

;;; Code:

;; Create another auto-generated init file so that the handcrafted one won't
;; be overwritten by Emacs.
(setq custom-file (expand-file-name "auto-generated.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Use straight for package management instead of package.el
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

;; Keyboard-centered user interface.
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Theme.
(set-frame-font "DejaVuSansMono Nerd Font Mono 11" nil t)
(use-package nord-theme
  :config
  (load-theme 'nord t))
(use-package powerline
  :config
  (powerline-default-theme))

;; Org-Roamd and more recent Org version from ELPA.
;; We have to delete the built-in packages in order for the package manager to
;; pull a newer version from ELPA.

(use-package org
  :init
  (require 'org))
(use-package org-roam)

;; Install counsel, ivy, and swipe for more complete completion.
(use-package counsel
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
  (define-prefix-command 'emacs-counsel-map)
  (global-set-key (kbd "C-c e") 'emacs-counsel-map)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-c e f") 'counsel-describe-function)
  (global-set-key (kbd "C-c e v") 'counsel-describe-variable)
  (global-set-key (kbd "C-c e l") 'counsel-find-library)
  (global-set-key (kbd "C-c e i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-c e u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c e j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c m") 'counsel-linux-app)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-c b") 'counsel-bookmark)
  (global-set-key (kbd "C-c o") 'counsel-outline)
  (global-set-key (kbd "C-c F") 'counsel-org-file))

;; Projectile for project navigation.
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

;; Help by showing all key bindings.
(use-package which-key
  :config
  (which-key-mode))

;; Use company mode for auto-completion suggestions.
(use-package company
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 4
	company-selection-wrap-around t))
(global-company-mode)

;; Magit for git integration
(use-package magit)

;;;;
;; Programming languages.
;;;;

;; Add line numbers in all programming modes.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Highlight numerals
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; flycheck for all languages to do diagnostics in-line.
(use-package flycheck
  :init (global-flycheck-mode))

;; YAML
(use-package yaml-mode)

;; Web Mode for all things web.
(use-package web-mode)

;; TypeScript
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
;; TSX
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
