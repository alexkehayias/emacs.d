;; This is needed or emacs 29 won't load
(defvar native-comp-deferred-compilation-deny-list nil)
;; Initialize straight.el and use-package.el
;; Assumes every use-package uses straight
(setq package-enable-at-startup nil)
;; Make startup faster by skipping this check
(setq straight-check-for-modifications nil)
;; Fix detached heads when bootstrapping straight.el
(setq straight-repository-branch "develop")
(setq straight-repository-user "radian-software")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-built-in-pseudo-packages
      '(emacs project xref))

(use-package emacs
  :init
  ;; Set garbage collector to a larger number
  (setq gc-cons-threshold 33554432)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Always turn on winner-mode
  (winner-mode))

;; Org-mode

(eval-when-compile
  (require 'cl))

(setq org-refile-path (or (getenv "ORG_REFILE_PATH") "~/Org/refile.org"))

(use-package org
  :config
  (setq org-directory "~/Org")

  ;; Maybe fix org hangs occasionally
  (setq org-element-use-cache nil)

  ;; When opening a file make sure everything is expanded
  ;; (setq org-startup-folded nil)

  ;; Always wrap lines
  (setq org-startup-truncated nil)

  ;; Hide markers like /emphasis/
  (setq org-hide-emphasis-markers t)

  ;; Show inline images
  (org-display-inline-images t t)

  ;; Don't show full size images otherwise it's too large when
  ;; displaying inline
  (setq org-image-actual-width nil)

  (defun org-image-resize (frame)
    (when (derived-mode-p 'org-mode)
      (setq org-image-actual-width (/ (window-pixel-width) 2))
      (org-redisplay-inline-images)))

  (add-hook 'window-size-change-functions 'org-image-resize)

  ;; Save notes into the logbook
  (setq org-log-into-drawer t)
  ;; Add logs for done to the logbook
  (setq org-log-done t)

  ;; Define global workflows
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "WAITING(w@/!)" "SOMEDAY(s!)" "|" "DONE(d!)" "CANCELED(c@/!)")))

  (setq org-tags-exclude-from-inheritance '("private"))

  ;; Set up custom agenda commands

  (setq org-agenda-custom-commands
           '(
             ("a" "Today's Agenda"
              ((org-ql-block '(and (category "strategic_areas"))
                        ((org-ql-block-header "STRATEGIC AREAS\n")))
               (org-ql-block '(and (category "weekly_goals"))
                        ((org-ql-block-header "WEEKLY GOALS\n")))
               (agenda "" ((org-agenda-skip-function
                            '(org-agenda-skip-entry-if 'regexp ":delegate:"))
                           (org-agenda-overriding-header "TODAY\n")
                           (org-agenda-span 1)
                           (org-agenda-skip-scheduled-if-done t)
                           (org-agenda-skip-timestamp-if-done t)
                           (org-agenda-skip-deadline-if-done t)
                           (org-agenda-start-day "+0d")
                           (org-agenda-repeating-timestamp-show-all nil)
                           (org-agenda-remove-tags t)
                           (org-agenda-prefix-format "  %-11:c %?-2 t%s")
                           (org-agenda-time)
                           (org-agenda-scheduled-leaders '("Scheduled: ""Sched.%2dx: "))
                           (org-agenda-deadline-leaders '("Deadline:  ""In %d days: " "%d days ago: "))
                           (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))
               (org-ql-block '(and (todo "NEXT") (not (tags "project_done")))
                             ((org-ql-block-header "NEXT")
                              (org-agenda-remove-tags t)
                              (org-super-agenda-groups '((:auto-priority t) (:auto-category t)))
                              (org-agenda-sorting-strategy
                               '(priority-down category-keep))))
               (org-ql-block '(and (todo "WAITING"))
                          ((org-ql-block-header "WAITING\n")
                           (org-agenda-prefix-format "  %-11:c %?b")
                           (org-agenda-todo-keyword-format "")))))
           ("r" "Daily Review"
            (
             (agenda "" (
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'regexp ":delegate:"))
                         (org-agenda-overriding-header "Daily Review\n")
                         ;; (org-agenda-start-with-log-mode '(closed))
                         (org-agenda-include-inactive-timestamps t)
                         (org-agenda-show-log 'only)
                         (org-agenda-span 1)
                         (org-agenda-skip-scheduled-if-done t)
                         (org-agenda-skip-timestamp-if-done t)
                         (org-agenda-skip-deadline-if-done t)
                         (org-agenda-start-day "+0d")
                         (org-agenda-repeating-timestamp-show-all nil)
                         (org-agenda-remove-tags t)
                         (org-agenda-prefix-format "  %-11:c %?-2 t%s")
                         (org-agenda-time)
                         (org-agenda-scheduled-leaders '("Scheduled: ""Sched.%2dx: "))
                         (org-agenda-deadline-leaders '("Deadline:  ""In %d days: " "%d days ago: "))
                         (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))
             (org-ql-block '(and (todo "TODO") (ts-inactive :on today))
                   ((org-ql-block-header "CREATED TODAY\n")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT" "WAITING")))
                    (org-super-agenda-groups '((:auto-category t)))))))

           ("u" "Unscheduled"
            (
             (org-ql-block '(todo "TODO")
                   ((org-ql-block-header "")
                    (org-agenda-sorting-strategy '(tsia-down))
                    ;; TODO only show work items
                    ;; TODO update this to show the inactive timestamps
                    (org-agenda-prefix-format '((todo . "  %-11:c %?-2 t%s")))
                    ))))))

  ;; Fix agenda lines wrapping
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (setq truncate-lines 1)))

  ;; Don't export headings with numbers
  (setq org-export-with-section-numbers nil)

  ;; gnuplot
  (local-set-key (kbd "M-C-g") 'org-plot/gnuplot)

  ;; Shortcut copy an internal link
  (global-set-key (kbd "C-c l") 'org-store-link)

  ;; Show hours:minutes instead of days:hours
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

  ;; Shortcut to jump to last running clock
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-jump-to-current-clock)

  ;; ;; Show org timestamps in 12h time
  (setq org-agenda-timegrid-use-ampm 1)

  ;; ;; Always show the current day in org agenda
  (setq org-agenda-time-grid (quote
                              ((daily today today)
                               (800 1000 1200 1400 1600 1800 2000)
                               "......."
                               "-----------------------------------------------------")))

  ;; Use longtable as the default table style when exporting
  (setq org-latex-default-table-environment "longtable")

  ;; Don't position tables in the center
  (setq org-latex-tables-centered nil)

  ;; Always use visual line mode to make it more like a document
  (add-hook 'org-mode-hook (lambda ()
                             (make-variable-buffer-local 'visual-line-mode)
                             (visual-line-mode)))

  ;; Show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)
  ;; For languages with significant whitespace like Python:
  (setq org-src-preserve-indentation t)

  ;; Timestamp new todos
  ;; (setq org-log-done 'time)

  (setq org-startup-indented t)

  (defun count-headings-in-region (start end)
    "Count the number of level 1 headings in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (let ((count 0))
          (while (re-search-forward "^\\* " nil t)
            (setq count (1+ count)))
          (message "Number of level 1 headings: %d" count)))))

  ;; Agenda
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c C-a") 'org-agenda)

  ;; In org agenda log view also show recurring tasks
  (setq org-agenda-log-mode-items '(closed))

  (defun org-archive-done-tasks ()
    "Archive all DONE and WONT-DO tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "+TODO=\"DONE\"" 'tree)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "+TODO=\"WONT-DO\"" 'tree))

  ;; Capture
  (global-set-key (kbd "C-c c") 'org-capture)

  (setq org-capture-templates
	(quote (("t" "To Do" entry (file org-refile-path)
		 "* TODO %?\n")
		("n" "Note" entry (file org-refile-path)
		 "* %? :note:\n")
		("m" "Meeting" entry (file org-refile-path)
		 "* Meeting w/%? %<%Y-%m-%d> :meeting:\n%U")
		("s" "Meeting" entry (file org-refile-path)
		 "* Meeting w/%? %<%Y-%m-%d> :meeting:sales:\n%U")
		("i" "Interview" entry (file org-refile-path)
		 "* Interview w/%? %<%Y-%m-%d> :interview:recruiting:\n%U"))))

  ;; Auto mark parent todos as done if childrend are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; Refile
  (setq org-default-notes-file org-refile-path)
  ;; Allow refile to the root of a file
  (setq org-refile-use-outline-path 'file)
  ;; Targets include this file and any file contributing to the agenda
  ;; up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
        			   (org-agenda-files :maxlevel . 9))))
  ;; Targets complete directly
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  ;; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Time format for clock table durations as h:mm
  (setq org-duration-format (quote h:mm))

  ;; Prompt for confirmation when exporting babel blocks
  (setq org-confirm-babel-evaluate t)

  ;; Don't run babel code during export This is important because I
  ;; often write down the code I used in code blocks but I don't want
  ;; to accidentally run it again
  (setq org-export-babel-evaluate nil)

  ;; THIS IS LOADING PROJECT.EL FOR SOME REASON
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((shell . t)
  ;;    (dot . t)
  ;;    (python . t)))

  (defun my/org-agenda-check-duplicates ()
    "Check org-agenda for duplicate headlines and display them in a new buffer."
    (interactive)
    (let ((duplicates (make-hash-table :test 'equal))
          (agenda-files (org-agenda-files)))
      (dolist (file agenda-files)
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (let ((headline (org-get-heading t t t t)))
               (when (and headline (not (string-blank-p headline)))
                 (let ((entry (gethash headline duplicates)))
                   (puthash headline (cons file entry) duplicates))))))
          ))
      (let (duplicate-list)
        (maphash (lambda (key locations)
                   (when (> (length locations) 1)
                     (push (cons key locations) duplicate-list)))
                 duplicates)
        (if duplicate-list
            (let ((buf (get-buffer-create "*Org Duplicates*")))
              (with-current-buffer buf
                (erase-buffer)
                (insert "* Duplicate Headlines:\n\n")
                (dolist (entry duplicate-list)
                  (let ((headline (car entry))
                        (files (cdr entry)))
                    (insert (format "** %s\n" headline))
                    (dolist (file files)
                      (insert (format "- [[file:%s::*%s][%s]]\n"
                                      file
                                      (org-link-escape headline)
                                      headline)))))
                (org-mode)
                (goto-char (point-min))
                (display-buffer buf)))
          (message "No duplicates found.")))))

  ;; [at] completion for org headlines
  (defun my/org-agenda-completions-at-point ()
    "Function to be used as `completion-at-point' in Org mode."
    (when (looking-back "@\\(\\(?:\\sw\\|\\s_\\|\\s-\\|\\s-:\\)+\\)")
      (defvar heading-to-id (make-hash-table :test 'equal))
      (let* ((start (match-beginning 1))
             (end (point))
             (input (match-string-no-properties 1))
             (candidates (org-ql-select
                           (org-agenda-files nil t)
                           (org-ql--query-string-to-sexp input)
                           ;; Avoid having to look up the ID again
                           ;; since we are visiting all the locations
                           ;; with org-ql anyway
                           :action (lambda ()
                                     (let* ((buf (buffer-name))
                                            (heading (org-get-heading t))
                                            (outline-path (org-get-outline-path t t))
                                            (key
                                             (org-format-outline-path outline-path nil buf))
                                            (destination `(,(org-id-get (point))
                                                           ,heading
                                                           ,outline-path
                                                           ,buf)))
                                       (puthash heading destination heading-to-id)
                                       heading))))
             (exit-function (lambda (heading status)
                              (when (eq status 'finished)
                                ;; The +1 removes the @ symbol
                                (delete-char (- (+ (length heading) 1)))
                                (let* ((dest (gethash heading heading-to-id))
                                       (heading-id (car dest))
                                       (heading-no-path (nth 1 dest))
                                       (heading-outline-path (nth 2 dest))
                                       (buf (nth 3 dest)))
                                  (insert
                                   (format "[[id:%s][%s]]"
                                           (or heading-id
                                               ;; If heading has no ID, create one
                                               (with-current-buffer buf
                                                 (goto-char (marker-position
                                                             (org-find-olp heading-outline-path t)))
                                                 (org-id-get-create)))
                                           heading-no-path)))))))
             (list start end candidates :exit-function exit-function))))

  (defun my/org-agenda-completion-hook ()
    "Configure org-mode for completion at point for org-agenda headlines."
    (add-to-list 'completion-at-point-functions 'my/org-agenda-completions-at-point))

  (add-hook 'org-mode-hook 'my/org-agenda-completion-hook))

;; macOS settings
(when (memq window-system '(mac ns))
  ;; Set the environment to the same as the shell
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))

  ;; Set option key to be meta key
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  ;; Always use Chrome when opening links
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)

  ;; Transparent title bar with no text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil))

;; Linux settings
(when (or
       (eq system-type 'gnu/linux)
       (not (display-graphic-p)))
  ;; Turn off the menu bar on top
  (menu-bar-mode -1))

;; Magit can run into an error where it won't open a commit buffer.
;; See https://github.com/magit/with-editor/issues/41
(defadvice server-ensure-safe-dir (around
                                   my-around-server-ensure-safe-dir
                                   activate)
  "Ignores any errors raised from server-ensure-safe-dir"
  (ignore-errors ad-do-it))

;; Max image size when using the builtin viewer
(setq max-image-size 50.0)

;; Disable visual bell
(setq visible-bell nil)

;; Disable backup files
(setq make-backup-files nil)
;; Disable auto save files
(setq auto-save-default nil)
;; Disable .# lock files
(setq create-lockfiles nil)

(when window-system
  ;; Disable pscroll bars in gui emacs
  (scroll-bar-mode -1)
  ;; Disable the tool bar in gui emacs
  (tool-bar-mode -1))

;; Highlight the current line
(global-hl-line-mode 1)

;; Change the scratch buffer message to nothing
(setq initial-scratch-message nil)

;; Don't suspend emacs with C-z
(global-unset-key [?\C-z])

;; Use iBuffer by default for C-x C-b which is more readable than default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Shortcut for occur
(global-set-key (kbd "C-x C-o") 'occur)

;; Shortcut for rectangle edits
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; Shortcut for jumping to next error with flymake (used by eglot)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

;; Copy file name to clipboard
(defun copy-file-name ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      ;; Save to clipboard
      (x-select-text filename)
      ;; Save to kill ring
      (kill-append filename 0))))

;; Screenshot as SVG using cairo (if emacs was built with --with-cairo)
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun disable-all-minor-modes ()
  (interactive)
  (mapc
   (lambda (mode-symbol)
     (when (functionp mode-symbol)
       ;; some symbols are functions which aren't normal mode functions
       (ignore-errors
         (funcall mode-symbol -1))))
   minor-mode-list))

;; Emdash
(defun insert-em-dash ()
  "Insert a em-dash"
  (interactive)
  (insert "—"))
(global-set-key [(meta _)] 'insert-em-dash)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show the time
(display-time-mode 1)

(use-package eglot
  :config
  ;; Fix breaking change introduced in
  ;; https://github.com/joaotavora/eglot/commit/d0a657e81c5b02529c4f32c2e51e00bdf4729a9e
  (defun eglot--major-mode (server) (car (eglot--major-modes server)))

  (add-to-list 'eglot-stay-out-of 'flyspell)

  ;; Better support for rust projects with multiple sub projects
  (defun my-project-try-cargo-toml (dir)
    (when-let* ((output
                 (let ((default-directory dir))
                   (shell-command-to-string "cargo metadata --no-deps --format-version 1")))
                (js (ignore-errors (json-read-from-string output)))
                (found (cdr (assq 'workspace_root js))))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions 'my-project-try-cargo-toml nil nil)

  (defun my-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions 'my-project-try-tsconfig-json nil nil)

  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio"))

  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:pycodestyle (:enabled :json-false)
                                    :pyflakes (:enabled t)
                                    :isort (:enabled t)
                                    :rope (:enabled t)
                                    :black (:enabled t)))))

  (defun my-project-try-pyproject-toml (dir)
    (when-let* ((found (locate-dominating-file dir "pyproject.toml")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions 'my-project-try-pyproject-toml nil nil)

  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  )

;; Projectile
(use-package projectile
  :config
  (projectile-mode)
  ;; Set the base keybinding
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Cache projectile projects
  (setq projectile-enable-caching t)
  ;; Defer to git if possible
  (setq projectile-indexing-method 'alien)
  ;; Limit projectile projects to these directories
  (setq projectile-project-search-path '("~/Projects/"))

  (defun projectile-term ()
    "Create an vterm at the project root"
    (interactive)
    (let ((root (projectile-project-root))
	  (buff-name (concat
		      (nth 1 (reverse (split-string (projectile-project-root) "/")))
		      " [term]")))
      (if (get-buffer buff-name)
	  (switch-to-buffer-other-window buff-name)
	(progn
	  (split-window-sensibly (selected-window))
	  (other-window 1)
	  (setq default-directory root)
          (vterm)
	  (rename-buffer buff-name t)))))
  (global-set-key (kbd "C-x M-t") 'projectile-term)

  (defun projectile-notes ()
    "Open org notes file at project root"
    (interactive)
    (let ((root (projectile-project-root))
	  (buff-name (concat
		      (nth 1 (reverse (split-string (projectile-project-root) "/")))
		      "notes.org")))
      (if (get-buffer buff-name)
	  (switch-to-buffer-other-window buff-name)
	(progn
          (if (file-exists-p (concat (projectile-project-root) "notes.org"))
              (progn
                (split-window-sensibly (selected-window))
                (other-window 1)
                (setq default-directory root)
                (find-file (concat root "notes.org")))
            (error "Notes file not found in this project"))))))
  (global-set-key (kbd "C-x M-n") 'projectile-notes)
)

;; Git using magit
(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
  ;; Fetch and revert buffer
  (defun rebase-and-revert ()
    "Fetch and rebase from git repo using Magit, then revert the current buffer."
    (interactive)
    (magit-run-git "fetch" "--all")
    (magit-run-git "rebase" "origin/main")
    (revert-buffer t t))
  (global-set-key (kbd "C-c r") 'rebase-and-revert)
  ;; Disable built in VC mode for better performance
  (setq vc-handled-backends nil)
  ;; Disable refreshing status for better performance
  (setq magit-refresh-status-buffer nil)
  ;; Show actual dates instead of relative dates in log view
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  ;; Always open the commit edit message in a new window instead of the
  ;; *magit* window so you can see the diff
  (add-to-list 'display-buffer-alist
	       '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
				      ((inhibit-same-window . t))))))

;; Completion

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)
;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)
;; Use fuzzy matching
(setq completion-styles '(flex basic partial-completion emacs22))

;; Web mode
(use-package web-mode
  :config
  (setq web-mode-js-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  ;; Javascript
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  ;; Typescript
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  ;; Turn off auto saving because js build tools hate temp files
  (add-hook 'web-mode-hook (lambda () (setq auto-save-default nil)))
  ;; HTML
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  ;; CSS
  (add-to-list 'auto-mode-alist '("\\.css$" . web-mode)))

(use-package tree-sitter
  :commands (tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist
  '(typescript-tsx-mode . tsx)))

(straight-use-package
 '(tsi :type git :host github :repo "orzechowskid/tsi.el"))

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
  (add-hook 'typescript-tsx-mode-hook
            (lambda ()
              (tree-sitter-mode)
              (tree-sitter-hl-mode)
              (tsi-typescript-mode))))

(use-package toml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode)))

(use-package rust-mode
  :config
  (setq rust-mode-treesitter-derive t))

;; Elisp
(use-package paredit
  :config
  (add-hook 'elisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

;; Yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;; Python
;; (use-package python-mode
;;   :config
;;   (setq python-shell-interpreter "python3.10")
;;   (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((python . t)))
;;   (setq-default py-shell-name "python3.10"
;;                 python-indent-offset 4
;;                 ;; Assumes the python source directory is in {PROJECT_ROOT}/src
;;                 py-python-command-args `("-i" "-c" "import os;import sys;sys.path.append(os.getcwd()+'src')")))

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (pyvenv-mode t)
;;   (setq pyvenv-mode-line-indicator
;;         '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
;;   ;; Set correct Python interpreter
;;   (setq pyvenv-post-activate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/ipython")))))
;;   (setq pyvenv-post-deactivate-hooks
;;         (list (lambda ()
;;                 (setq python-shell-interpreter "ipython"))))
;;   )

;; (use-package pyenv-mode
;;   :config
;;   (pyenv-mode)
;;   (defun projectile-pyenv-mode-set ()
;;     "Set pyenv version matching project name."
;;     (let ((project (projectile-project-name)))
;;       (if (member project (pyenv-mode-versions))
;;           (pyenv-mode-set project)
;;         (pyenv-mode-unset))))

;;   (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

;; Format line numbers nicely
(setq linum-format (quote " %3d "))

;; Sass-mode
(use-package sass-mode
  :config (setq sass-tab-width 2))

;; Markdown
(use-package markdown-mode
  :config
  ;; Remove keymapping that conflicts with flymake next error
  (add-hook 'markdown-mode-hook
            (lambda ()
              (local-unset-key (kbd "M-n"))
              (local-unset-key (kbd "M-p"))))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

(defun writeroom-setup ()
  (interactive)
  ;; Use custom font face for this buffer only
  (defface tmp-buffer-local-face
    '((t :family "Maple Mono" :height 160 :weight 'bold))
    "Temporary buffer-local face")
  (buffer-face-set 'tmp-buffer-local-face)
  ;; Use a skinny cursor
  (make-local-variable 'cursor-type)
  (setq cursor-type 'bar)
  ;; Add extra line spacing
  (setq-local line-spacing 0.5)
  ;; Disable hl-line-mode if it's on
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1))
  )

;; ;; Nice writing layout
(use-package writeroom-mode
  :config
  (customize-set-value
   'writeroom-fullscreen-effect
   nil)
  (add-hook 'writeroom-mode-hook 'writeroom-setup))

;; Unity
(use-package glsl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.shader$" . glsl-mode)))

;; Linum mode shortcut
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)

;; iEdit mode
(global-set-key (kbd "C-x ;") 'iedit-mode)

;; Web browser (eww)
(global-set-key (kbd "C-x M-w") 'eww)

;; Disable auto fill mode because it's annoying
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; Shortcuts for going forward and backwards cycling windows
;; (global-set-key (kbd "C-x p") 'other-window)
;; (global-set-key (kbd "C-x o") 'previous-multiframe-window)

;; Expand region
(use-package expand-region
  :config
  (global-set-key (kbd "\C-x\ =") 'er/expand-region))

;; Avy
(use-package avy
  :config
  (global-set-key (kbd "C-c SPC") 'avy-goto-char-timer))

;; Auto refresh all buffers when files change ie git branch switching
(global-auto-revert-mode t)

;; Don't use any character as the vertical border divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ? ))

;; Gui emacs still sets a line color in between
(set-face-background 'vertical-border "gray32")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

;; When at the end of a buffer don't jump, scroll
(setq scroll-step 1 scroll-conservatively 10000)
(setq scroll-margin 0)

(use-package clojure-mode)

;; Kibit for Clojure
;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
   Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

;; Rainbow parantheses
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Cider settings
(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook
	    '(lambda ()
	       (set-variable 'nrepl-server-command
			     "lein with-profile dev repl")))
  ;; Disable popup stacktraces except in repl
  (setq nrepl-popup-stacktraces nil)
  (setq nrepl-popup-stacktraces-in-repl t)

  ;; Disable auto-selection of the error buffer when it's displayed:
  (setq cider-auto-select-error-buffer nil)

  ;; Instructions for M-x cider-jack-in-clojurescript
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

;; Flyspell mode
(use-package flyspell
  :config
  (setq ispell-program-name "/opt/homebrew/bin/aspell")
  ;; (add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'coffee-mode-hook (lambda () (flyspell-prog-mode)))
  (add-hook 'hbs-mode-hook (lambda () (flyspell-prog-mode)))
  ;; Disable in org-mode because it is too slow
  ;; (add-hook 'org-mode-hook (lambda () (flyspell-mode -1)))
  )

;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

(use-package all-the-icons
  :ensure t)

(use-package gnuplot
  :defer t
  :after org
  :config
  ;; Enable gnuplot in babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t))))

(use-package htmlize)

;; Org export
(use-package ox-reveal
  :defer t)

(use-package ox-jira
  :defer t)

(use-package ox-hugo
  :after org
  :config
  ;; Assume all static files are images for now otherwise this
  ;; defaults to /ox-hugo/mypicture.png which is ugly
  (setq org-hugo-default-static-subdirectory-for-externals "img"))

;; Heavily modified based on https://github.com/novoid/title-capitalization.el/blob/master/title-capitalization.el
(defun title-capitalization (str)
  "Convert str to title case"
  (interactive)
  (with-temp-buffer
    (insert str)
    (let* ((beg (point-min))
           (end (point-max))
	   ;; Basic list of words which don't get capitalized according to simplified rules
	   ;; http://karl-voit.at/2015/05/25/elisp-title-capitalization/
           (do-not-capitalize-basic-words '("a" "ago" "an" "and" "as" "at" "but" "by" "for"
                                            "from" "in" "into" "it" "next" "nor" "of" "off"
                                            "on" "onto" "or" "over" "past" "so" "the" "till"
                                            "to" "up" "yet"
                                            "n" "t" "es" "s"))
	   ;; If user has defined 'my-do-not-capitalize-words, append to basic list
           (do-not-capitalize-words (if (boundp 'my-do-not-capitalize-words)
                                        (append do-not-capitalize-basic-words my-do-not-capitalize-words )
                                      do-not-capitalize-basic-words)))
      ;; Go to begin of first word
      (goto-char beg)
      (setq continue t)

      ;; Go through the region, word by word
      (while continue
        (let ((last-point (point)))
          (let ((word (thing-at-point 'word)))
            (if (stringp word)
                ;; Capitalize current word except when it is list member
                (if (and (member (downcase word) do-not-capitalize-words)
                         ;; Always capitalize first word
                         (not (= (point) 1)))
                    (downcase-word 1)

                  ;; If it's an acronym, don't capitalize
                  (if (string= word (upcase word))
                      (progn
                        (goto-char (+ (point) (length word) 1)))
                    (capitalize-word 1)))))

          (skip-syntax-forward "^w" end)

          ;; Break if we are at the end of the buffer
          (when (= (point) last-point)
            (setq continue nil))))

      ;; Always capitalize the last word
      (backward-word 1)

      (let ((word (thing-at-point 'word)))
        (if (and (>= (point) 0)
                 (not (member (or word "s")
                              '("n" "t" "es" "s")))
                 (not (string= word (upcase word))))
            (capitalize-word 1))))

    (buffer-string)))

;; Org roam
;; These are specified so they can be dynamically configured
;; by calling emacs in batch mode in a CI context
(setq org-roam-notes-path (or (getenv "ORG_ROAM_NOTES_PATH") "~/Org/notes"))
(setq org-roam-publish-path (or (getenv "ORG_ROAM_PUBLISH_PATH") "~/Projects/zettel"))

(defun my/org-roam-capture-set-file-name (&rest r)
  "Set the file name as the buffer name if possible. Fixes eglot not
     working on capture and dailies."
  (when-let* ((buf-name (buffer-name)))
    (setq-local buffer-file-name
                (format "%s/%s"
                        (file-truename org-roam-notes-path)
                        (string-replace "CAPTURE-" "" (buffer-name))))))

(use-package org-roam
  :after (org)
  ;; :bind (:map org-mode-map
  ;;             ("M-." . org-open-at-point)
  ;;             ("M-," . org-mark-ring-goto))
  :bind  (;; ("C-c n l" . org-roam-buffer-toggle)
          ;; ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n c" . org-roam-capture)
          ("C-c n j" . org-roam-dailies-capture-today)
          ("C-c n r" . org-roam-random-note)
          ("C-c n u" . org-roam-unlinked-references)
          ("C-c n e" . org-roam-to-hugo-md)
          ("C-c n i" . org-roam-node-insert))

  :custom
  (org-roam-mode-section-functions
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         #'org-roam-unlinked-references-section))
  :hook
  ;; Need to add advice after-init otherwise they won't take
  ((after-init . (lambda ()
                   (advice-add 'org-roam-capture
                               :after
                               'my/org-roam-capture-set-file-name)

                   (advice-add 'org-roam-dailies-capture-today
                               :after
                               'my/note-taking-init)

                   (advice-add 'org-roam-dailies-capture-today
                               :after
                               'my/org-roam-capture-set-file-name))))

  :init
  (setq org-roam-directory org-roam-notes-path)
  ;; Needed to supress update warning
  (setq org-roam-v2-ack t)
  ;; Fix Emacs 28.2 can't find `sqlite`
  (setq org-roam-database-connector 'sqlite-builtin)

  (setq org-roam-dailies-directory org-roam-notes-path)
  ;; Include tags in note search results
  (setq org-roam-node-display-template "${title}      ${tags}")

  ;; Customize the org-roam buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  ;; Find only projects
  (defun my/org-roam-node-find-project ()
    (interactive)
    (org-roam-node-find
     nil nil
     (lambda (node)
       (seq-contains-p (org-roam-node-tags node) "project"))))
  (global-set-key (kbd "C-c n p") 'my/org-roam-node-find-project)

  ;; Only show the paragraph with the link in the org-roam buffer
  (defun my/preview-fetcher ()
    (let* ((elem (org-element-context))
           (parent (org-element-property :parent elem)))
      ;; TODO: alt handling for non-paragraph elements
      (string-trim-right (buffer-substring-no-properties
                          (org-element-property :begin parent)
                          (org-element-property :end parent)))))

  (setq org-roam-preview-function #'my/preview-fetcher)
  ;; These functions need to be in :init otherwise they will not be
  ;; callable in an emacs --batch context which for some reason
  ;; can't be found in autoloads if it's under :config
  (defun my/org-roam--extract-note-body (file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (first (org-element-map (org-element-parse-buffer) 'paragraph
               (lambda (paragraph)
                 (let ((begin (plist-get (first (cdr paragraph)) :begin))
                       (end (plist-get (first (cdr paragraph)) :end)))
                   (buffer-substring begin end)))))))
  ;; Include backlinks in org exported notes not tagged as private or
  ;; draft or section
  (defun my/org-roam--backlinks-list (id file)
    (--reduce-from
     (concat acc (format "- [[id:%s][%s]]\n  #+begin_quote\n  %s\n  #+end_quote\n"
                         (car it)
                         (title-capitalization (org-roam-node-title (org-roam-node-from-id (car it))))
                         (my/org-roam--extract-note-body (org-roam-node-file (org-roam-node-from-id (car it))))))
     ""
     (org-roam-db-query
      (format
       ;; The percentage sign needs to be escaped twice because there
       ;; is two format calls—once here and the other by emacsql
       "SELECT Q.id FROM (SELECT links.source AS id, group_concat(tags.tag) AS alltags FROM links LEFT OUTER JOIN tags ON links.source = tags.node_id WHERE links.type = '\"id\"' AND links.dest = '\"%s\"' GROUP BY links.source) Q JOIN nodes ON Q.id = nodes.id WHERE nodes.level = 0 AND (alltags IS NULL OR (','||alltags||',' NOT LIKE '%%%%,\"private\",%%%%' AND ','||alltags||',' NOT LIKE '%%%%,\"draft\",%%%%' AND ','||alltags||',' NOT LIKE '%%%%,\"section\",%%%%'))"
       id))))

  (defun file-path-to-md-file-name (path)
    (let ((file-name (first (last (split-string path "/")))))
      (concat (first (split-string file-name "\\.")) ".md")))

  (defun file-path-to-slug (path)
    (let* ((file-name (file-name-nondirectory path))
           (note-name (car (last (split-string file-name "--"))))
           (title (first (split-string note-name "\\."))))
      (replace-regexp-in-string (regexp-quote "_") "-" title nil 'literal)))

  ;; Org export is very slow when processing org-id links. Override it
  ;; to skip opening the file and loading all modes.
  (defun my/org-export--collect-tree-properties (data info)
    "Extract tree properties from parse tree.

    DATA is the parse tree from which information is retrieved.  INFO
    is a list holding export options.

    Following tree properties are set or updated:

    `:headline-offset' Offset between true level of headlines and
                       local level.  An offset of -1 means a headline
                       of level 2 should be considered as a level
                       1 headline in the context.

    `:headline-numbering' Alist of all headlines as key and the
                          associated numbering as value.

    `:id-alist' Alist of all ID references as key and associated file
                as value.

    Return updated plist."
    ;; Install the parse tree in the communication channel.
    (setq info (plist-put info :parse-tree data))
    ;; Compute `:headline-offset' in order to be able to use
    ;; `org-export-get-relative-level'.
    (setq info
          (plist-put info
                     :headline-offset
                     (- 1 (org-export--get-min-level data info))))
    ;; From now on, properties order doesn't matter: get the rest of the
    ;; tree properties.
    (org-combine-plists
     info
     (list :headline-numbering (org-export--collect-headline-numbering data info)
           :id-alist
           (org-element-map data 'link
             (lambda (l)
               (and (string= (org-element-property :type l) "id")
                    (let* ((id (org-element-property :path l))
                           (file (org-id-find-id-file id)))
                      (and file (cons id (file-relative-name file))))))))))

  (advice-add 'org-export--collect-tree-properties :override #'my/org-export--collect-tree-properties)

  ;; No notes use anchor links so ignore this to speed it up
  (defun my/org-hugo-link--headline-anchor-maybe (link)
    "")
  (advice-add 'org-hugo-link--headline-anchor-maybe :override #'my/org-hugo-link--headline-anchor-maybe)

  ;; ox-hugo doesn't set the `relref` path correctly so we need to
  ;; tell it how to do it
  (defun my/org-id-path-fix (strlist)
    (file-name-nondirectory strlist))

  (advice-add 'org-export-resolve-id-link :filter-return #'my/org-id-path-fix)

  ;; Fetches all org-roam files and exports to hugo markdown
  ;; files. Adds in necessary hugo properties
  ;; e.g. HUGO_BASE_DIR. Ignores notes tagged as private or draft
  (defun org-roam-to-hugo-md ()
    (interactive)
    ;; Make sure the author is set
    (setq user-full-name "Alex Kehayias")

    ;; Don't include any files tagged as private or
    ;; draft. The way we filter tags doesn't work nicely
    ;; with emacsql's DSL so just use a raw SQL query
    ;; for clarity
    (let ((notes (org-roam-db-query "SELECT id, file FROM (SELECT nodes.id, nodes.file, nodes.level, group_concat(tags.tag) AS alltags FROM nodes LEFT OUTER JOIN tags ON nodes.id = tags.node_id GROUP BY nodes.file) WHERE level = 0 and (alltags is null or (','||alltags||',' not like '%%,\"private\",%%' and ','||alltags||',' not like '%%,\"draft\",%%'))")))
      (-map
       (-lambda ((id file))
         ;; Use temporary buffer to prevent a buffer being opened for
         ;; each note file.
         (with-temp-buffer
           (message "Working on: %s" file)

           (insert-file-contents file)

           ;; Change relative links that work within emacs to view an image to
           ;; absolute paths for use with exported markdown to html.
           (goto-char (point-min))
           (while (re-search-forward "\\[\\[\\.\\/img\\([^]]*\\)\\]\\]" nil t)
             (replace-match "[[/img\\1]]" nil nil))

           ;; Adding these tags must go after file content because it
           ;; will include a :PROPERTIES: drawer as of org-roam v2
           ;; which must be the first item on the page

           ;; Add in hugo tags for export. This lets you write the
           ;; notes without littering HUGO_* tags everywhere
           ;; HACK:
           ;; org-export-output-file-name doesn't play nicely with
           ;; temp buffers since it attempts to get the file name from
           ;; the buffer. Instead we explicitely add the name of the
           ;; exported .md file otherwise you would get prompted for
           ;; the output file name on every note.
           (goto-char (point-min))
           (re-search-forward ":END:")
           (newline)
           (insert
            (format "#+HUGO_BASE_DIR: %s\n#+HUGO_SECTION: ./\n#+HUGO_SLUG: %s\n#+EXPORT_FILE_NAME: %s\n"
                    org-roam-publish-path
                    (file-path-to-slug file)
                    (file-path-to-md-file-name file)))

           ;; If this is a placeholder note (no content in the
           ;; body) then add default text. This makes it look ok when
           ;; showing note previews in the index and avoids a headline
           ;; followed by a headline in the note detail page.
           (if (eq (my/org-roam--extract-note-body file) nil)
               (progn
                 (goto-char (point-max))
                 (insert "\n/This note does not have a description yet./\n")))

           ;; Add in backlinks (at the end of the file) because
           ;; org-export-before-processing-hook won't be useful the
           ;; way we are using a temp buffer
           (let ((links (my/org-roam--backlinks-list id file)))
             (if (not (string= links ""))
                 (progn
                   (goto-char (point-max))
                   (insert (concat "\n* Links to this note\n") links))))

           (org-hugo-export-to-md)))
       notes)))

  ;; Cache node find completions
  ;; FIX: This breaks backlinks because it doesn't handle different
  ;; sources of completion requests
  ;; Copied from https://github.com/Konubinix/Devel/blob/30d0184db0a61f46ca35e102302d707fef964a8c/elfiles/config/after-loads/KONIX_AL-org-roam.el#L770-L787
  ;; (defvar my/org-roam-node-read--completions/cache nil "Memory cache of the list of nodes")
  ;; (defvar my/org-roam-node-read--completions/cache-time nil "The time when the cache was last taken")
  ;; (defun my/org-roam-node-read--completions/cache (orig-fun &rest args)
  ;;   (when (or
  ;;          (not my/org-roam-node-read--completions/cache)
  ;;          (not my/org-roam-node-read--completions/cache-time)
  ;;          (time-less-p
  ;;           my/org-roam-node-read--completions/cache-time
  ;;           (file-attribute-modification-time (file-attributes org-roam-db-location))
  ;;           )
  ;;          )
  ;;     (message "Computing the org-roam-node-read--completions")
  ;;     (setq my/org-roam-node-read--completions/cache-time (current-time))
  ;;     (setq my/org-roam-node-read--completions/cache (apply orig-fun args))
  ;;     )
  ;;   my/org-roam-node-read--completions/cache
  ;;   )
  ;; (advice-add #'org-roam-node-read--completions :around #'my/org-roam-node-read--completions/cache)

  :config
  (defun my/org-id-update-org-roam-files ()
    "Update Org-ID locations for all Org-roam files."
    ;; https://org-roam.discourse.group/t/org-roam-v2-org-id-id-link-resolution-problem/1491/4
    (interactive)
    (org-id-update-id-locations (org-roam-list-files)))

  (setq org-roam-capture-templates
	(quote (("d" "Default" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}.org\" (current-time) t)"
                          "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n\n")
                 :unnarrowed t)
                ;; Private notes are not exported
                ("p" "Private" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}.org\" (current-time) t)"
                          "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: private\n\n")
                 :unnarrowed t)
                ;; Sections notes are exported as their own page
                ("s" "Section" plain
                  "%?"
                  :if-new (file+head
                           "${slug}.org"
                           "#+TITLE: ${title}\n#+FILETAGS: section\n\n")
                  :unnarrowed t)
                ;; Projects track long running work of related tasks
                ("P" "Project" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--project-${slug}.org\" (current-time) t)"
                          "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+CATEGORY: ${slug}\n#+FILETAGS: private project\n\n")
                 :unnarrowed t)
                ;; An entity is a person or place used for backlinks as a pseudo CRM
                ("e" "Entity" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--entity-${slug}.org\" (current-time) t)"
                          "#+TITLE: @${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: private entity\n\n")
                 :unnarrowed t))))

  ;; Journaling setup
  (setq org-roam-dailies-directory "")

  (setq org-roam-dailies-capture-templates
	(quote (("d" "Default" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--journal.org\" (current-time) t)"
                          "#+TITLE: Journal %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: private journal\n\n\n")
                 :unnarrowed t))))

  ;; Use writeroom mode when capturing new notes. Hide the ugly
  ;; preamble of org attributes by scrolling up.
  (defun my/note-taking-init (&rest r)
    (with-current-buffer (current-buffer)
      (writeroom-mode)))

  ;; When jumping to the note and jumping back, close the other buffer.
  ;; TODO: This doesn't work for multiple jumps
  (defun close-buffer-after-mark-goto (around f)
    (winner-undo))

  (advice-add 'org-mark-ring-goto :around #'close-buffer-after-mark-goto)

  ;; Run org-roam sync on load
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :defer t
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Macro for running a function repeatedly in the background
;; https://github.com/punchagan/dot-emacs/blob/master/punchagan.org
(defmacro run-with-timer-when-idle (secs idle-time repeat-time function &rest args)
  "Run a function on timer, but only when idle."
  `(run-with-timer
    ,secs
    ,repeat-time
    (lambda () (run-with-idle-timer ,idle-time nil ,function ,@args))))

;; Much better terminal emulator. Requires that emacs is installed
;; --with-modules to work.
(use-package vterm
  :defer t)

(defun new-term (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (vterm)
  (rename-buffer buffer-name t))

;; Speed up ansi-term in emacs 24.x by skipping guessing left-to-right input
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  (setq bidi-paragraph-direction 'left-to-right))

;; Shortcut to jump word and ignore underscore
(define-key global-map (kbd "M-F") 'forward-sexp)
(define-key global-map (kbd "M-B") 'forward-sexp)

;; Highlight changes is removed after save
(defun highlight-changes-remove-after-save ()
  "Remove previous changes after save."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (highlight-changes-remove-highlight (point-min) (point-max)))))

;; Center text mode

(define-minor-mode center-buffer-mode
  "Minor mode for centering buffer to 80 characters"
  :init-value nil
  :global nil
  :lighter " CB"
  (if center-buffer-mode
      (progn (setq-local center-margins (/ (- (window-width) 80) 2))
             (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                                 center-margins
                                 center-margins)
             (add-hook 'window-configuration-change-hook 'center-buffer-mode-resize nil t))
    (progn (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                               nil
                               nil)
           (remove-hook 'window-configuration-change-hook 'center-buffer-mode-resize t))))

(defun center-buffer-mode-resize ()
  "Called when the window size changes, to recalculate buffer centering."
  (interactive)
  (center-buffer-mode-clear)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                      (/ (- (window-width) 80) 2)
                      (/ (- (window-width) 80) 2)))

(defun center-buffer-mode-clear ()
  "Clear buffer centering."
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                      nil
                      nil))

(define-key global-map (kbd "C-c M-t") 'center-buffer-mode)

;; Enable system clipboard
(setq x-select-enable-clipboard t)

;; Start emacs without all the fanfare
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; Rename file and buffer in one shot
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Shells should have color
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Json mode
(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  (add-hook 'json-mode 'flymake-json-load))

;; Fix pasting from a buffer to term
(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))

(add-hook 'term-mode-hook 'my-term-hook)

;; Shortcut for incrementing and decrementing a number under the cursor
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "-0123456789")
  (or (looking-at "[-0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (10+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "-0123456789")
  (or (looking-at "[-0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun increment-and-eval-block ()
  (interactive)
  (increment-number-at-point)
  (nrepl-eval-expression-at-point))

(defun decrement-and-eval-block ()
  (interactive)
  (decrement-number-at-point)
  (nrepl-eval-expression-at-point))

(global-set-key (kbd "C-c +") 'increment-and-eval-block)
(global-set-key (kbd "C-c -") 'decrement-and-eval-block)

;; Set a redo command
(global-set-key (kbd "C-x M-r") 'repeat-complex-command)

(defun describe-last-function ()
  (interactive)
  (describe-function last-command))

;; Save a recorded macro with a name
(defun save-macro (name)
  "Takes a name as argument and save the last defined macro under
   this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the
                                        ; macro
  (kmacro-name-last-macro name)         ; use this name for
                                        ; the macro
  (find-file user-init-file)            ; open ~/.emacs
                                        ; or other user init file
  (goto-char (point-max))               ; go to
                                        ; the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

;; browse-kill-ring with M-y
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

;; Prevent accidental upcase/downcase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Unload a theme before enabling another
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Use doom theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-monokai-octagon t))

(use-package nerd-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 40))

(use-package terraform-mode)

;; Set default font
;; When using gui emacs with emacsclient, the following makes sure the
;; window is opened with the correct font size otherwise it will
;; revert to 13px for font size
(add-to-list 'default-frame-alist '(font . "Cascadia Code"))
;; Make the default face the same font
(set-face-attribute 'default nil :font "Cascadia Code" :weight 'medium)
(set-face-attribute 'default nil :height 140)

;; Keyboard shortcut for using a big screen
(setq big-screen nil)

(defun toggle-big-screen ()
  (interactive)
  (if big-screen
      (progn
	(setq big-screen nil)
	(set-face-attribute 'default nil :height 140))
    (progn
      (set-face-attribute 'default nil :height 160)
      (setq big-screen 1))))
(global-set-key (kbd "C-x M-b") 'toggle-big-screen)

;; Add gh codespaces ssh method support for tramp editing
;; e.g. C-x C-f /ghcs:codespace-name:/path/to/file :)
;; cp'd from https://github.com/anticomputer/emacs-codeql
(let ((ghcs (assoc "ghcs" tramp-methods))
      (ghcs-methods '((tramp-login-program "gh")
                      (tramp-login-args (("codespace") ("ssh") ("-c") ("%h")))
                      (tramp-remote-shell "/bin/sh")
                      (tramp-remote-shell-login ("-l"))
                      (tramp-remote-shell-args ("-c")))))
  ;; just for debugging the methods
  (if ghcs (setcdr ghcs ghcs-methods)
    (push (cons "ghcs" ghcs-methods) tramp-methods)))

;; provide codespace name completion for ghcs tramp method
;; use C-j if you use ivy to kick in host completion
(defun my/tramp-parse-codespaces (&optional nop)
  (let ((results '())
        (codespaces
         (split-string
          (shell-command-to-string
           "gh codespace list --json name -q '.[].name'"))))
    (dolist (name codespaces)
      ;; tramp completion expects a list of (user host)
      (add-to-list 'results (list nil name)))
    results))

(tramp-set-completion-function "ghcs" '((my/tramp-parse-codespaces "")))

;; Speed up Tramp by using the same connection
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes"))

;; Speed up Tramp by turning off version control when remote
(defun my/vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))

(add-hook 'find-file-hook 'my/vc-off-if-remote)

(use-package org-remark
  :config
  (define-key org-remark-mode-map (kbd "C-c m m") #'org-remark-mark)
  (define-key org-remark-mode-map (kbd "C-c m o") #'org-remark-open)
  (define-key org-remark-mode-map (kbd "C-c m n") #'org-remark-view-next)
  (define-key org-remark-mode-map (kbd "C-c m p") #'org-remark-view-prev)
  (define-key org-remark-mode-map (kbd "C-c m r") #'org-remark-remove)
  (org-remark-global-tracking-mode +1))

(use-package org-download
  :config

  (defun my/org-file-is-private ()
    (let ((tags (car (cdr (car (org-collect-keywords '("FILETAGS")))))))
      (if (stringp tags)
          (member "private" (split-string tags))
        nil)))

  (defun my/org-download-set-dir ()
    "Set `org-download-image-dir` to the directory of the current
        buffer's file."
    (interactive)
    (when (buffer-file-name)
      (if (my/org-file-is-private)
          ;; Save downloaded images to a directory that indicates it is
          ;; private. This will not be exported as part of the notes
          ;; publishing pipeline.
          (setq-local org-download-image-dir
                      (concat (file-name-directory
                               (buffer-file-name))
                              "img/private"))
        ;; Save downloaded images to the normal img directory. These
        ;; will be published as part of the notes publishing pipeline
        (setq-local org-download-image-dir
                    (concat (file-name-directory
                             (buffer-file-name))
                            "img")))))

  :hook
  ((org-mode . my/org-download-set-dir)))



(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://mastodon.social")
  (setq mastodon-active-user "alexkehayias"))

(use-package ox-json)

(use-package org-super-agenda
  :config
  (org-super-agenda-mode t))

;; This fixes a CI failure currently with org-ql
;; https://github.com/alphapapa/org-ql/issues/345
(defalias 'byte-run--set-speed
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''speed (list 'quote val))))

(use-package org-ql
  :straight (org-ql :type git
                    :host github
                    :repo "alphapapa/org-ql")
  :config
  (defun my/org-ql-find ()
    (interactive)
    (org-ql-find (org-agenda-files nil t)))

  (define-key global-map (kbd "C-c s") 'my/org-ql-find))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :config (setq chatgpt-shell-openai-key (or (getenv "OPENAI_API_KEY") "")))

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key (or (getenv "OPENAI_API_KEY") "")))

(use-package git-auto-commit-mode
  :config
  (setq-default gac-debounce-interval 1))

(use-package org-modern
  :after org
  :config

  (global-org-modern-mode)

  (defun my/org-modern-style ()
    (interactive)
    (setq-local line-spacing 0.125))

  (add-hook 'org-mode-hook 'my/org-modern-style)
  (add-hook 'org-agenda-mode-hook 'my/org-modern-style))

(use-package gptel
  :config
  (setq gptel-api-key (or (getenv "OPENAI_API_KEY") ""))
  (setq gptel-default-mode 'org-mode)
  ;; Ollama setup
  (gptel-make-ollama "Ollama"
    :stream t
    :host "localhost:11434"
    :models '("llama3:8b"
              "deepseek-r1:8b")))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :after (embark)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-x C-r" . consult-ripgrep)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; TODO: Add a source for org capture buffers used by org-mode and
  ;; org-roam which are buffers with a prefix of CAPTURE
  ;; (consult--buffer-query :mode 'org-mode)
  ;; (defvar last-buffer-source
  ;;   (list :name     "Last Buffer"
  ;;         :category 'buffer
  ;;         :narrow   ?o
  ;;         :face     'consult-buffer
  ;;         :history  'buffer-name-history
  ;;         :state    #'consult--buffer-state
  ;;         :items
  ;;         (lambda ()
  ;;           (consult--buffer-query :as #'buffer-name))))

  ;; Alternatively always show previous buffer first in the list of
  ;; buffers
  ;; (defvar org-source
  ;;   (list :name     "Org Buffer"
  ;;         :category 'buffer
  ;;         :narrow   ?o
  ;;         :face     'consult-buffer
  ;;         :history  'buffer-name-history
  ;;         :state    #'consult--buffer-state
  ;;         :new
  ;;         (lambda (name)
  ;;           (with-current-buffer (get-buffer-create name)
  ;;             (insert "#+title: " name "\n\n")
  ;;             (org-mode)
  ;;             (consult--buffer-action (current-buffer))))
  ;;         :items
  ;;         (lambda ()
  ;;           (consult--buffer-query :mode 'org-mode :as #'buffer-name))))

  ;; (add-to-list 'consult-buffer-sources 'org-source 'append)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Don't show a preview unless I want to
  (setq consult-preview-key "C-,")

  ;; Narrow search
  (setq consult-narrow-key ">"))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  ;; :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  ;; :hook
  ;; (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   ;; (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize consult-org-roam-forward-links :preview-key "C-,")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n f" . consult-org-roam-file-find)
   ("C-c n l" . consult-org-roam-backlinks)
   ("C-c n L" . consult-org-roam-backlinks-recursive)
   ("C-c n s" . consult-org-roam-search))

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Enable Corfu globally.
  ;; See also `corfu-exclude-modes'.
  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)

  ;; Don't use corfu in comint-mode or it messes with org-ai-shell
  (add-hook 'comint-mode-hook
            (lambda ()
              (corfu-mode -1))))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; Display macros inline in buffers
(add-to-list 'font-lock-extra-managed-props 'display)

(font-lock-add-keywords
 'org-mode
 '(("\\({{{[a-zA-Z#%)(_-+0-9]+}}}\\)" 0
    `(face nil display
           ,(format "%s"
                    (let* ((input-str (match-string 0))
                          (el (with-temp-buffer
                                (insert input-str)
                                (goto-char (point-min))
                                (org-element-context)))
                          (text (org-macro-expand el org-macro-templates)))
                      (if text
                          text
                        input-str)))))))

;; Experimental ox-notion export backend
(add-to-list 'load-path (expand-file-name "ox-notion" user-emacs-directory))
(use-package ox-notion :ensure nil :straight nil)

;; Experimental org-ai shell
(add-to-list 'load-path (expand-file-name "org-ai-shell" user-emacs-directory))
(use-package org-ai-shell
  :ensure nil
  :straight nil)

;; Experimental sticky buffer
(add-to-list 'load-path (expand-file-name "sticky-buffer-mode" user-emacs-directory))
(use-package sticky-buffer-mode
  :ensure nil
  :straight nil
  :config
  (sticky-buffer-mode 1))


;; Extended org-edit-special to support more block types based on edit-indirect
;; - edit-indirect by Fanael https://github.com/Fanael/edit-indirect
;; - org-edit-indirect by agzam https://github.com/agzam/.spacemacs.d
(add-to-list 'load-path (expand-file-name "edit-indirect" user-emacs-directory))
(use-package edit-indirect :ensure nil :straight nil)
(add-to-list 'load-path (expand-file-name "org-edit-indirect" user-emacs-directory))
(use-package org-edit-indirect :ensure nil :straight nil)

;; Store customizations in a separate file
(setq custom-file "~/.emacs.d/.customizations.el")
(load custom-file t)

(provide 'init)
