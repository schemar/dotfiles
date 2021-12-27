;;; init -- Emacs init file

;;; Commentary:
;; Define the init file

;;; Code:

;; Create another auto-generated init file so that the handcrafted one won't
;; be overwritten by Emacs.
(setq custom-file (expand-file-name "auto-generated.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Define and initialize additional package repositories.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use use-package to simplify package management.
;; Will automatically install itself and any missing packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Keyboard-centered user interface.
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Theme.
(set-frame-font "DejaVuSansMono Nerd Font Mono 11" nil t)
(use-package nord-theme
  :ensure t
  :config (load-theme 'nord t))
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

;; Projectile for project navigation
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Helm configuration for finding files, buffers, and so on.
(use-package helm
  :config
  (require 'helm-config)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  :init
  (helm-mode 1)
  :bind
  (("M-x"     . helm-M-x) ;; Evaluate functions
   ("C-x C-f" . helm-find-files) ;; Open or create files
   ("C-x b"   . helm-mini) ;; Select buffers
   ("C-x C-r" . helm-recentf) ;; Select recently saved files
   ("C-c i"   . helm-imenu) ;; Select document heading
   ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
   :map helm-map
   ("C-z" . helm-select-action)
   ("<tab>" . helm-execute-persistent-action)))

(use-package helm-projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; Show help for shortcuts.
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle 0.5
	which-key-idle-delay 50)
  (which-key-setup-minibuffer))

;; Use company mode for auto-completion suggestions.
(use-package company
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 4
	company-selection-wrap-around t))
(global-company-mode)

;; Magit for git integration
(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit))

;;;;
;; Programming languages.
;;;;

;; Add line numbers in all programming modes.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; flycheck for all languages to do diagnostics in-line.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; YAML
(use-package yaml-mode)

;; Web Mode for all things web.
(use-package web-mode)

;; TypeScript
(use-package tide
  :ensure t
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
