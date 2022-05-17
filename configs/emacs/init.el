;;; package --- schemar emacs
;;; Commentary:
;; Carefully hand copy-pasted Emacs config.
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

;; Load this at the top so that the white frame disappears as quickly as possible.
;;; Theme:
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

;;
;;; Emacs fundamentals:

(defvar schemar/default-font-size 105)
(defvar schemar/default-variable-font-size 125)

(defun schemar/display-startup-time ()
  "Display the time it took Emacs to load."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'schemar/display-startup-time)

(setq inhibit-startup-message t)

;; I do not need two spaces after a sentence. One is fine.
(setq sentence-end-double-space nil)

;; Always end a file with a newline.
(setq require-final-newline t)

;; Remove UI stuff.
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(menu-bar-mode -1) ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Show the column in the mode line.
(column-number-mode)

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(setq pgtk-wait-for-event-timeout 0.001)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages. `doom/open-scratch-buffer' provides a better
      ;; scratch buffer anyway.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

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

;; Restart Emacs:
(use-package restart-emacs
  :commands (restart-emacs))

;; which-key:
(use-package which-key
  :hook (after-init . which-key-mode))

(use-package helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))


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

;; ;; Spelling:
;; (use-package ispell
;;   :straight (:type built-in)
;;   :defer t
;;   :init
;;   (setq ispell-program-name "aspell"
;;         ispell-extra-args '("--sug-mode=ultra"
;;                             "--run-together")))
;; (use-package flyspell
;;   :straight (:type built-in)
;;   :hook ((org-mode
;;           markdown-mode
;;           TeX-mode
;;           rst-mode
;;           mu4e-compose-mode
;;           message-mode
;;           git-commit-mode) .
;;          flyspell-mode)

;;   :hook ((yaml-mode
;;           conf-mode
;;           prog-mode) .
;;          flyspell-prog-mode)
;;   :config
;;   (provide 'ispell))

;; (use-package flyspell-correct
;;   :after flyspell
;;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
;; (use-package flyspell-correct-ivy
;;   :after flyspell-correct)
;; (use-package flyspell-lazy)

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

;;
;;; Code and development:

;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like go-mode).
(setq-default indent-tabs-mode nil
              tab-width 4)

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

;; Web
(use-package web-mode
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.[lh]?eex\\'"
  :mode "\\.sface\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :init
  ;; If the user has installed `vue-mode' then, by appending this to
  ;; `auto-mode-alist' rather than prepending it, its autoload will have
  ;; priority over this one.
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)
  :mode "\\.vue\\'")

(use-package css-mode)

(use-package sass-mode)

(use-package counsel-css)

(use-package company-web)

;; TypeScript / JavaScript:
(use-package rjsx-mode)

(use-package typescript-mode)

(use-package js2-refactor)

(use-package npm-mode)

(use-package add-node-modules-path)

(use-package tide)

(use-package xref-js2)

;; LSP:
(use-package lsp-mode
  :hook (((html-mode-local-vars
           web-mode-local-vars
           nxml-mode-local-vars) . lsp-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-install-server))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
        ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)

;;
;;; Global key-bindings:

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun schemar/find-file ()
  "Find a file within a project if we are currently in a project.
Use normal find file functionality otherwise."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
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

   "q" '(:ignore t :which-key "Quit")
   "qr" '(restart-emacs :which-key "Restart")

   "p" '(:ignore t :which-key "Projects")
   "pp" '(counsel-projectile-switch-project :which-key "Switch project")
   "pa" '(counsel-projectile-add-known-project :which-key "Add project")))

;;
;;; Clean up:

;; Make sure to set up garbage collection!
;; It was disabled in `early-init.el'. 
(use-package gcmh
  :init
  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (setq gcmh-idle-delay 'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 128 1024 1024))  ; 128mb
  :config
  (gcmh-mode 1))

;;; init.el ends here
