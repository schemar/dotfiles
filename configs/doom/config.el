;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; General settings

;; Change local leader key to be quicker.
(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Martin Schenck"
      user-mail-address "martinschenck@fastmail.com")

;; Allow i3 to always float "everywhere" by identifying it by its name.
(setq emacs-everywhere-frame-name-format "emacs-everywhere-float")

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
      doom-variable-pitch-font (font-spec :family "Bitter" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'. To
;; enable line numbers, set it to `t'.
(setq display-line-numbers-type t)

;; Convert images from unknown formats to PNG.
(setq image-use-external-converter t)

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

;;; :emacs dired
;; Shortcut to open ranger.
(map! :leader
      :prefix "f"
      :desc "Ranger" "m" #'ranger)

(after! ranger
  ;; Show hidden files in ranger windows.
  (setq ranger-show-hidden t))

;;; :core projectile
;; Go to "other" file based on test file, not file extension (was the default).
(after! projectile
  (map! :leader
        :desc "Find other file" "p o"
        #'projectile-toggle-between-implementation-and-test))

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
;; Allow point to move to end-of-line character.
(setq evil-move-beyond-eol t)

(after! evil
  ;; Flip (horizontal) splitting.
  (map! :leader :m "w s" #'evil-window-vsplit
        :leader :m "w v" #'evil-window-split)

  ;; Switch windows with control key.
  (map! "C-h" #'evil-window-left
        "C-j" #'evil-window-down
        "C-k" #'evil-window-up
        "C-l" #'evil-window-right)
  )

;;; :tools lsp
(after! lsp-mode
  ;; Show where we are at the top of the buffer.
  (setq lsp-headerline-breadcrumb-enable t)

  ;; Make sure that TypeScript files only get formatted once, with eslint when
  ;; present.
  (setq-hook! 'typescript-mode-hook +format-with-lsp nil)

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

;;; :tools dap
(after! dap-mode
  (dap-register-debug-template
   "Node Run TS"
   (list :type "node"
         :cwd nil
         :request "launch"
         :program nil
         :name "Node Run TS"
         :sourceMaps t
         :runtimeArgs ["--no-lazy" "-r" "ts-node/register"])))

;;; :term vterm
;; TODO: Disabled as it currently breaks Doom's "toggle" for vterm.
;; (after! vterm
;;   (setq vterm-buffer-name-string "vterm %s"))

;;; Gemini/Gopher
(use-package! elpher
  :defer t)

;;
;;; :lang org

;; Use org mode when using Emacs everywhere.
(setq emacs-everywhere-major-mode-function #'org-mode)

;; Where my org(-roam) files are stored.
(setq org-directory "~/Documents/org/")

(after! org
  ;; No need to be so bold.
  (custom-set-faces!
    `(org-level-1 :weight normal :foreground ,(doom-color 'blue))
    `(org-level-2 :weight normal :foreground ,(doom-color 'magenta))
    `(org-level-3 :weight normal :foreground ,(doom-color 'dark-blue))
    `(org-level-4 :weight normal :foreground ,(doom-color 'cyan))
    `(org-level-5 :weight normal :foreground ,(doom-color 'violet))
    `(link        :weight normal :foreground ,(doom-color 'blue)))

  ;; Doom uses org-appear. We don't want all emphasis markers always.
  (setq org-hide-emphasis-markers t)

  ;; Do not hightlight long lines in org-mode, as the links are usually longer
  ;; than they look.
  (setq-hook! 'org-mode-hook whitespace-style
              (remove 'lines-tail whitespace-style))

  ;; Use mixed pitch mode for variable pitch font within org mode.
  ;; TODO: Ensure the "size" of doom's variable pitch font is applied.
  ;;(add-hook! 'org-mode-hook #'mixed-pitch-mode)

  ;; Nicer folding and initial behavior.
  (setq org-startup-folded 'show2levels
        org-ellipsis " […]")

  ;; if there is a #+ATTR.*: width="200", resize to 200, otherwise resize to 400
  (setq org-image-actual-width '(400))

  ;; Extend the priorities so that B is above none and D is low.
  ;; See also setting of `org-fancy-priorities-list' after the `org' block.
  (setq org-priority-lowest ?D
        org-priority-default ?C)

  ;; Set the face of the new lowest priority to not be red.
  (push '(?D . success) org-priority-faces)

  ;; Customize the bullet appearance of org headings.
  (setq org-superstar-headline-bullets-list
        '(9672 9671 9673 9675 9654 9655))

  ;; Set up keywords incl. when to be asked to add a note.
  (setq org-todo-keywords '((type "TODO(t!)"
                                  "WAIT(w@/!)"
                                  "HOLD(h@/!)"
                                  "PROJ(p!)"
                                  "|"
                                  "DONE(d!)"
                                  "DELEGATED(l@)"
                                  "KILL(k@)")))
  ;; Explicitly set done keywords for org-ql.
  (setq org-done-keywords '("DONE" "DELEGATED" "KILL"))

  ;; Explicitly track when a task was closed (as a property that is also used by
  ;; `ox-hugo').
  (setq org-log-done 'time)

  ;; Make sure that tasks with sub-tasks or a sub-checklist cannot be marked
  ;; done, if the sub-tasks/list aren't done.
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;; Insert state change notes and time stamps into a drawer.
  (setq org-log-into-drawer t)

  ;; Include running timer in clock table
  (setq org-clock-report-include-clocking-task t)

  ;;
  ;; Agenda
  ;;

  ;; Show Monday as first day of week.
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  ;; Start the agenda view today (default in Doom was `-3d').
  (setq org-agenda-start-day nil)

  (defun schemar/org-agenda-files ()
    "Recursively looks for all org files in the org directory."
    (directory-files-recursively org-directory "\\.org$"))

  ;; Org agenda should get files from the org directory as well as the daily
  ;; directory of `org-roam-dailies'.
  (setq org-agenda-files (schemar/org-agenda-files))

  ;; Add new files to the list of agenda files. This ensures that the agenda
  ;; will list new TODOs from new files. The advice expects the new file to be
  ;; saved to disk before the agenda command is invoked!
  (defadvice! schemar/add-new-agenda-files (&rest _args)
    :before #'org-agenda
    (setq org-agenda-files (schemar/org-agenda-files)))

  ;; Org key bindings.
  (map! :map org-agenda-mode-map
        :localleader :desc "log mode" "l" #'org-agenda-log-mode)

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
  (defun org-roam-backlinks-sort (a b)
    "Custom sorting function for backlinks A and B.

This function overrides org-roam's default sorting function for
backlinks in the roam buffer. It will always sort date nodes
before other nodes. It will sort date nodes newest to
oldest (descending). After the date nodes, it will sort all other
nodes in alphabetical order (ascending)."
    (let* ((title-a (org-roam-node-title (org-roam-backlink-source-node a)))
           (title-b (org-roam-node-title (org-roam-backlink-source-node b)))
           (date-regexp (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)))
           (a-matches-date (string-match date-regexp title-a))
           (b-matches-date (string-match date-regexp title-b)))
      (cond ((and a-matches-date b-matches-date) (string< title-b title-a))
            (a-matches-date t)
            (b-matches-date nil)
            (t (string< title-a title-b)))))

  (defadvice! schemar/org-set-tags-from-roam (orig-fun &rest args)
    "Offers tags from org-roam when setting regular org tags.

Org-roam's `org-roam-tag-add' only adds tags to org-roam nodes.
With this advice, the tag completion offers tags known to
org-roam when adding regular org tags.

It is much faster than the alternative `(setq org-complete-tags-always-offer-all-agenda-tags t)'"
    :around #'org-set-tags-command
    (let ((org-current-tag-alist (mapcar #'list (org-roam-tag-completions))))
      (apply orig-fun args)))

  (setq org-roam-directory (file-truename org-directory)
        org-roam-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("c" "clock in" entry "* %u %?"
           :target (file+olp "%<%Y%m%d%H%M%S>-${slug}.org" ("Clocking"))
           :clock-in t
           :prepend t
           :jump-to-captured t)
          ("t" "task" entry   "* TODO %?\n:LOGBOOK:\n- State \"TODO\"       from              %U \\\\\n  Created while at %a\n:END:"
           :target (file+olp "%<%Y%m%d%H%M%S>-${slug}.org" ("Tasks"))
           :prepend t)))

  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n
* Report\n
#+BEGIN: clocktable :scope schemar/org-agenda-files :hidefiles t :narrow 40 :tags t :link t :stepskip0 t :fileskip0 t :block %<%Y-%m-%d> :sort (1 . ?a)\n
#+END
"))))

  (defun schemar/org-capture-clock ()
    "Org roam capture with key c"
    (interactive)
    (org-roam-capture nil "c"))

  (defun schemar/org-capture-task ()
    "Org roam capture with key t"
    (interactive)
    (org-roam-capture nil "t"))

  ;; Move scratch buffer over from `:leader x' to `:leader X'.
  (map! :leader :desc "Pop up scratch buffer" "X" #'doom/open-scratch-buffer)
  ;; Replace org capturing at `:leader X' with custom capture at `:leader x'.
  ;; Unbind x first so that it can be used as a prefix after.
  (map! :leader "x" nil)
  (map! :leader
        :prefix ("x" . "Capturing")
        :desc "Clock" "c" #'schemar/org-capture-clock
        :desc "Task"  "t" #'schemar/org-capture-task))

(after! deft
  (setq deft-extensions '("org")
        deft-directory org-directory
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

;; TODO: Enable fancy priorities with org-ql.
(use-package! org-ql
  :after org)

(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Today"
           :time-grid t)
          (:name "Done"
           :todo ("DONE" "DELEGATED" "KILL"))
          (:name "Futurice"
           :tag "futurice")
          (:name "Private"
           :tag "private")))
  ;; Need to fix header map of super agenda to not override evil bindings.
  (setq org-super-agenda-header-map (make-sparse-keymap)))

;;; git-auto-commit
(setq gac-automatically-push-p t
      gac-automatically-add-new-files-p t
      gac-debounce-interval 10
      gac-silent-message-p t)

;;; Todoist
(after! todoist
  (map! :leader :desc "Todoist" :n "d" #'todoist)

  ;; Instead, set up shortcuts to actions directly?
  (map! :localleader :map todoist-mode-map
        :desc "Task menu" :n "t" #'todoist-task-menu
        :desc "Project menu" :n "p" #'todoist-project-menu)
  ;; TODO: How to handle API token?
  ;; (setq todoist-token "")
  )
