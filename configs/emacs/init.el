;;; package --- schemar emacs
;;; Commentary:
;; Carefully handcrafted Emacs config.
;; Takes parts from Emacs from Scratch, Doom Emacs, and Spacemacs.

;;
;;; Package management:

;; straight.el package manager:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure that use-package always uses straight:
(setq straight-use-package-by-default t)

;; use-package convenience macro:
(straight-use-package 'use-package)

;;
;;; Emacs fundamentals:

(defvar schemar/default-font-size 105)
(defvar schemar/default-variable-font-size 125)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun schemar/display-startup-time ()
  "Display the time it took Emacs to load."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'schemar/display-startup-time)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Fonts:
(set-face-attribute 'default nil :font "MonoLisa Nerd Font" :height schemar/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "MonoLisa Nerd Font" :height schemar/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Roboto" :height schemar/default-variable-font-size :weight 'regular)

;;
;;; Overarching packages:

;; which-key:
(use-package which-key
  :hook (after-init . which-key-mode))

;; Theme:
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors:
  (doom-themes-visual-bell-config)
  ;; Enable custom treemacs theme:
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Mode line:
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Icons:
(use-package all-the-icons
  :if (display-graphic-p))

;; Ivy, Counsel, and Swiper (counsel pulls in the others as dependencies):
(use-package counsel
  :config
  (ivy-mode 1))
(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))
(use-package all-the-icons-ivy-rich
  :after ivy
  :if (display-graphic-p)
  :init
  (all-the-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :after all-the-icons-ivy-rich
  :init
  (ivy-rich-mode 1))
(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode))

;; Company:
(use-package company
  :after evil
  :hook
  (company-mode . evil-normalize-keymaps)
  (after-init . global-company-mode))
(use-package company-dict
  :after company
  :defer t)
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink))))))

;;
;;; Spelling and syntax checking:
;; TODO: Spelling is still not working as intended.

;; Spelling:
(use-package ispell
  :straight (:type built-in)
  :defer t
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"
                            "--run-together")))
(use-package flyspell
  :straight (:type built-in)
  :hook ((org-mode
          markdown-mode
          TeX-mode
          rst-mode
          mu4e-compose-mode
          message-mode
          git-commit-mode)
         flyspell-mode)

  :hook ((yaml-mode
          conf-mode
          prog-mode)
         flyspell-prog-mode)
  :config
  (provide 'ispell))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
(use-package flyspell-correct-ivy
  :after flyspell-correct)
(use-package flyspell-lazy)

;; Syntax:

;;; Terminal:
(use-package vterm)

;;; Evil:

(use-package undo-tree
  :hook (evil-local-mode . turn-on-undo-tree-mode))

(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  ;; `evil-collection' assumes `evil-want-keybinding' is set to
  ;; `nil' before loading `evil' and `evil-collection'
  ;; @see https://github.com/emacs-evil/evil-collection#installation
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter)

;;

;;
;;; Dired:

(use-package dired
  :straight (:type built-in)
  :commands dired-jump
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask)
  :config
  (setq dired-listing-switches "-ahl -v --group-directories-first"))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-git-info
  :hook (dired-after-readin . dired-git-info-auto-enable)
  :init (setq dgi-auto-hide-details-p nil))

(use-package ranger
  :after dired
  :init (setq ranger-override-dired t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config

  ;; display icons with colors
  (setq all-the-icons-dired-monochrome nil))

(use-package fd-dired
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired))

;;; Git and VCS:

;; Magit:
(use-package magit)
(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?")) ; make colon optional


;;
;;; Projects:

;; Projectile:
(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :custom ((projectile-completion-system 'ivy))
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;;
;;; Global key-bindings:

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun schemar/find-file ()
  "Find a file within a project if we are currently in a project.
Use normal find file functionality otherwise."
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-find-file)
    (counsel-find-file)))

(defun schemar/grep ()
  "Grep for content in files."
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-rg)
    (counsel-rg)))

(use-package general
  :after evil
  :config
  (general-define-key
   "M-/" 'evilnc-comment-or-uncomment-lines)

  (general-create-definer schemar/leader-definer
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (schemar/leader-definer

   "SPC" '(schemar/find-file :which-key "Find file")
   "/" '(schemar/grep :which-key "Grep")

   "w" '(evil-window-map :which-key "Windows")

   "f" '(:ignore t :which-key "Files")
   "ff" '(counsel-find-file :which-key "Find file")
   "fs" '(save-buffer :which-key "Save buffer")

   "b" '(:ignore t :which-key "Buffers")
   "bb" '(ivy-switch-buffer :which-key "List buffers")
   "bd" '(kill-current-buffer :which-key "Close buffer")

   "p" '(:ignore t :which-key "Projects")
   "pp" '(counsel-projectile-switch-project :which-key "Switch project")
   "pa" '(counsel-projectile-add-known-project :which-key "Add project")))

;;
;;; Clean up:

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
