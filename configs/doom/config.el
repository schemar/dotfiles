;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; General settings

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Martin Schenck"
      user-mail-address "martinschenck@fastmail.com")

;; Allow i3 to always float "everywhere".
(setq emacs-everywhere-frame-name-format "emacs-everywhere")

;; Always keep a few lines around the point visible.
(setq scroll-margin 8)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "MonoLisa Nerd Font" :size 10.5)
      doom-variable-pitch-font (font-spec :family "Roboto" :size 12.5))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'. To
;; enable line numbers, set it to `t'.
(setq display-line-numbers-type t)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Allow 100 characters before highlighting a line as too long
(setq whitespace-line-column 100)

;; Enable showing of whitespace.
(global-whitespace-mode +1)

;; Use pop-up search for default search with slash.
(map! :desc "Search" :n "/" #'+default/search-buffer)

;;; Packages
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; :core projectile
;; Go to "other" file based on test file, not file extension (was the default).
(after! projectile
  (map! :leader :desc "Find other file" "p o" #'projectile-toggle-between-implementation-and-test))

;;; :ui treemacs
;; Set the icons to be the same as in dired (all-the-icons).
(setq doom-themes-treemacs-theme "doom-colors")
;; Do not use variable pitch font for treemacs.
(setq doom-themes-treemacs-enable-variable-pitch nil)

(after! treemacs
  ;; Better git mode (requires Python 3).
  (setq treemacs-git-mode 'deferred))

;;; :editor evil
;; Focus new window after splitting.
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :checkers spell
;; Disable flyspell messages to improve performance.
(setq flyspell-issue-message-flag nil
      flyspell-issue-welcome-flag nil)

;; Default to English.
(after! ispell (setq ispell-dictionary "en"))

;;; :completion company
(after! company
  (map! :map company-active-map
        ;; Use `TAB' to select element, instead of cycling through candidates.
        "<tab>" #'company-complete-selection
        ;; Use `ESC' to exit company mode without leaving insert mode.
        "<escape>" #'company-abort))

;;; :tools lsp
(after! lsp-mode
  ;; Show where we are at the top of the buffer.
  (setq lsp-headerline-breadcrumb-enable t))

;; Make sure that TypeScript files only get formatted once, with eslint when present.
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(after! lsp-mode
  (defun my/eslint-format ()
    (interactive
     (if-let ((eslint (-first (lambda (wks)
                                (eq 'eslint (lsp--client-server-id
                                             (lsp--workspace-client wks))))
                              (lsp-workspaces))))
         (with-lsp-workspace eslint
           (lsp-format-buffer))
       (lsp-format-buffer))))
  (setq-hook! 'typescript-mode-hook +format-with 'my/eslint-format))

;;; Tree Sitter
;; See also https://github.com/hlissner/doom-emacs-private/blob/master/modules/ui/tree-sitter/config.el
(use-package! tree-sitter
  ;; Enable tree sitter for programming modes:
  :hook (prog-mode . tree-sitter-mode)
  ;; Syntax highlighting:
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)  (defadvice! doom-tree-sitter-fail-gracefully-a (orig-fn &rest args)
                                  "Don't break with errors when current major mode lacks tree-sitter support."
                                  :around #'tree-sitter-mode
                                  (condition-case e
                                      (apply orig-fn args)
                                    (error
                                     (unless (string-match-p (concat "^Cannot find shared library\\|"
                                                                     "^No language registered\\|"
                                                                     "cannot open shared object file")
                                                             (error-message-string e))
                                       (signal (car e) (cadr e)))))))

;;; Gemini/Gopher
(use-package! elpher
  :defer t)

;;; :lang org
;; Use org-mode in the *scratch* buffer (`SPC x')
(setq doom-scratch-initial-major-mode 'org-mode)

;; Use org mode when using Emacs everywhere.
(setq emacs-everywhere-major-mode-function #'org-mode)

;; Where my org(-roam) files are stored.
(setq org-directory "~/Documents/org/")

(after! org
  ;; Disable latex in org mode as it slows down editing too much :(
  (setq org-highlight-latex-and-related nil)

  ;; Nicer folding and initial behavior.
  (setq org-startup-folded 'show2levels
        org-ellipsis " […]")

  ;; Extend the priorities so that B is above none and D is low.
  ;; See also setting of `org-fancy-priorities-list' after the `org' block.
  (setq org-priority-lowest ?D
        org-priority-default ?C)

  ;; Set up keywords incl. when to be asked to add a note.
  (setq org-todo-keywords '((type "TODO(t!)" "WAIT(w@/!)" "HOLD(h@/!)" "PROJ(p!)" "|" "DONE(d!)" "DELEGATED(l@)" "KILL(k@)")))

  ;; Explicitly track when a task was closed (as a property that is also used by `ox-hugo').
  (setq org-log-done 'time)

  ;; Make sure that tasks with sub-tasks or a sub-checklist cannot be marked done, if the
  ;; sub-tasks/list aren't done.
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;; Insert state change notes and time stamps into a drawer.
  (setq org-log-into-drawer t)

  ;; Include running timer in clock table
  (setq org-clock-report-include-clocking-task t)

  ;; Disable auto-completion with `company' in org-mode.
  (setq-hook! org-mode company-idle-delay nil)

  ;;
  ;; Agenda
  ;;

  ;; Do not show DONE items in the agenda.
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  ;; Show Monday as first day of week.
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  ;; Start the agenda view today (default in Doom was `-3d').
  (setq org-agenda-start-day nil)

  ;; Org agenda should get files from the org directory as well as the daily directory of
  ;; `org-roam-dailies'.
  (setq org-agenda-files
        (directory-files-recursively "~/Documents/org/" "\\.org$"))

  ;; Org key bindings.
  (map! :map org-agenda-mode-map
        :localleader :desc "log mode" "l" #'org-agenda-log-mode)

  ;; Also add a hook so that the list is re-created on every agenda.
  ;; It could be the case that new files were added in the meantime, which would not be considered
  ;; by org-agenda otherwise.
  ;; Note the removal of files that contain =.#= in their name.
  ;; These are temporary files which I assume are created by org or org-roam.
  ;; Org-agenda would complain any time it doesn't find these files anymore.
  ;; Therefore we take them out of the list of files.
  (setq-hook! org-agenda-mode
    org-agenda-files
    (cl-delete-if
     (lambda (f)
       (string-match-p "\\.#" f))
     (directory-files-recursively "~/Documents/org/" "\\.org$")))

  ;; [[https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html][Source]].
  ;; Vulpea functions are also available [[https://github.com/d12frosted/vulpea][here]].
  (setq org-agenda-prefix-format
        '((agenda . " %i %(vulpea-agenda-category 12)%?-12t% s")
          (todo . " %i %(vulpea-agenda-category 12) ")
          (tags . " %i %(vulpea-agenda-category 12) ")
          (search . " %i %(vaulpea-agenda-category 12) ")))

  (defun vulpea-agenda-category (&optional len)
    "Get category of item at point for agenda.

    Category is defined by one of the following items:

    - CATEGORY property
    - TITLE keyword
    - TITLE property
    - filename without directory and extension

    When LEN is a number, resulting string is padded right with
    spaces and then truncated with ... on the right if result is
    longer than LEN.

    Usage example:

      (setq org-agenda-prefix-format
            '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

    Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (vulpea-buffer-prop-get "title"))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  category)
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result))
        result)))
  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))
  )

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("" "" "" "")))

(map! :leader
      :desc "Go to today"
      "n r t" #'org-roam-dailies-goto-today)

(after! org-roam
  (setq org-roam-directory (file-truename "~/Documents/org")
        org-roam-dailies-directory "daily/"
        ;; Completion slows down the org buffers too much
        org-roam-completion-everywhere nil
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  )

(after! deft
  (setq deft-extensions '("org")
        deft-directory "~/Documents/org"
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  ;; Use #+title as title:
  (advice-add 'deft-parse-title :override
              (lambda (file contents)
                (if deft-use-filename-as-title
	            (deft-base-filename file)
	          (let* ((case-fold-search 't)
	                 (begin (string-match "title: " contents))
	                 (end-of-begin (match-end 0))
	                 (end (string-match "\n" contents begin)))
	            (if begin
	                (substring contents end-of-begin end)
	              (format "%s" file)))))))

(use-package! org-ql
  :after org)

(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Today"
           :time-grid t)
          (:name "Futurice"
           :tag "futurice")
          (:name "Private"
           :tag "private")))
  ;; Need to fix header map of super agenda to not override evil bindings.
  (setq org-super-agenda-header-map (make-sparse-keymap)))
