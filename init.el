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

  ;; Save notes into the logbook
  (setq org-log-into-drawer t)
  ;; Add logs for done to the logbook
  (setq org-log-done t)

  ;; Define global workflows
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "WAITING(w@/!)" "SOMEDAY(s!)" "|" "DONE(d!)" "CANCELED(c@/!)")))

  ;; Set up custom agenda commands

  (setq org-agenda-custom-commands
           '(
             ("a" "Today's Agenda"
              ((tags "CATEGORY=\"goals\""
                     ((org-agenda-overriding-header "GOALS\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "")
                      (org-agenda-todo-keyword-format "")))
               (agenda "" (
                           (org-agenda-skip-function
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
               (todo "NEXT"
                     ((org-agenda-overriding-header "NEXT")
                      (org-agenda-remove-tags t)
                      (org-super-agenda-groups '((:auto-priority t)))
                      (org-agenda-sorting-strategy
                       '(priority-down category-keep))))
               (todo "WAITING"
                     ((org-agenda-overriding-header "WAITING\n")
                      (org-agenda-remove-tags t)))
               (tags-todo "delegate"
                          ((org-agenda-overriding-header "DELEGATED\n")
                           (org-agenda-prefix-format "  %-11:c %?b")
                           (org-agenda-todo-keyword-format "")))))
           ("r" "Daily Review"
            (
             (agenda "" (
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'regexp ":delegate:"))
                         (org-agenda-overriding-header "Daily Review\n")
                         (org-agenda-start-with-log-mode '(closed))
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
             (todo ""
                   ((org-agenda-overriding-header "")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("NEXT" "WAITING")))
                    (org-super-agenda-groups '((:auto-category t)))))))

           ("u" "Unscheduled"
            (
             (todo "TODO"
                   ((org-agenda-overriding-header "")
                    (org-agenda-sorting-strategy '(tsia-down))
                    ;; TODO only show work items
                    ;; TODO update this to show the inactive timestamps
                    (org-agenda-prefix-format '((todo . "  %-11:c %?-2 t%s")))
                    ))))
           )
           )

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
		 "* TODO %?\n%U")
		("n" "Note" entry (file org-refile-path)
		 "* %? :note:\n%U\n%a\n")
		("m" "Meeting" entry (file org-refile-path)
		 "* Meeting w/%? %<%Y-%m-%d> :meeting:\n%U")
		("s" "Meeting" entry (file org-refile-path)
		 "* Meeting w/%? %<%Y-%m-%d> :meeting:sales:\n%U")
		("i" "Interview" entry (file org-refile-path)
		 "* Interview w/%? %<%Y-%m-%d> :interview:\n%U"))))

  ;; Auto mark parent todos as done if childrend are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; Refile
  (setq org-default-notes-file "~/Org/refile.org")
  ;; Allow refile to the root of a file
  (setq org-refile-use-outline-path 'file)
  ;; Targets include this file and any file contributing to the agenda
  ;; up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
        			   (org-agenda-files :maxlevel . 9))))
  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  ;; Use IDO for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)
  ;; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Time format for clock table durations as h:mm
  (setq org-duration-format (quote h:mm))

  ;; Don't prompt for confirmation when exporting babel blocks
  (setq org-confirm-babel-evaluate nil)

  (defun org-headline-completions ()
    "Return a list of all headlines in the current Org mode buffer."
    (let ((headlines '()))
      (dolist (file (org-agenda-files))
        (with-current-buffer (find-file-noselect file)
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (headline)
              (push (org-element-property :raw-value headline) headlines)))))
      headlines))

  (defun my/org-agenda-completions-at-point ()
    "Function to be used as `completion-at-point' in Org mode."
    (when (looking-back "@\\(\\(?:\\sw\\|\\s_\\)+\\)")
      (let* ((start (match-beginning 1))
             (end (point))
             (completions (org-headline-completions)))
        (list start end completions))))

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

;; Shortcut for rgrep using ripgrep because it's faster
(global-set-key (kbd "C-x C-r") 'ripgrep-regexp)

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

;; Nice startup screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome back Ender")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq show-week-agenda-p t))

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

;; Git using magit
(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
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
  (add-hook 'web-mode-hook '(lambda () (setq auto-save-default nil)))
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
              (tsi-typescript-mode)))
  (add-hook 'typescript-mode-hook #'eglot-ensure))

(use-package toml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode)))

(use-package rust-mode
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook #'eglot-ensure))

(setq grammarly-client-id (or (getenv "GRAMMARLY_CLIENT_ID") "client_testing"))

(use-package project :ensure t)

(use-package eglot
  :after (project projectile)
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

  ;; Handle projects not in the root folder of a repo
  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))

  (add-hook 'project-find-functions 'my-project-try-cargo-toml nil nil)

  (defun my-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions 'my-project-try-tsconfig-json nil nil)

  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio"))

  (defclass eglot-grammarlylsp (eglot-lsp-server) ()
    :documentation "Grammarly Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-grammarlylsp))
    "Passes the initializationOptions required to run the server."
    `(:clientId ,grammarly-client-id))

  (setq-default eglot-workspace-configuration
                '((:pylsp . ((:plugins .
                                       ((:pycodestyle . ((:enabled . :json-false)))
                                        (:pyflakes . ((:enabled . t)))
                                        (:black . ((:enabled . t)))))))))

  (defun my-project-try-pyproject-toml (dir)
    (when-let* ((found (locate-dominating-file dir "pyproject.toml")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions 'my-project-try-pyproject-toml nil nil)

  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

  (add-to-list 'eglot-server-programs
               `(markdown-mode . (eglot-grammarlylsp ,(executable-find "grammarlylsp"))))

  (add-to-list 'eglot-server-programs
               `(org-mode . (eglot-grammarlylsp ,(executable-find "grammarlylsp")))))

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
(use-package python-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (add-hook 'python-mode-hook #'eglot-ensure)
  (setq-default py-shell-name "python3"))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/ipython")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "ipython")))))

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
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-hook 'markdown-mode-hook #'eglot-ensure))

(defun writeroom-setup ()
  (interactive)
  ;; Use custom font face for this buffer only
  (defface tmp-buffer-local-face
    '((t :family "Space Mono" :height 150))
    "Temporary buffer-local face")
  (buffer-face-set 'tmp-buffer-local-face)
  ;; Use a skinny cursor
  (make-local-variable 'cursor-type)
  (setq cursor-type 'bar)
  ;; Add extra line spacing
  (setq-local line-spacing 0.5))

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
(global-set-key (kbd "C-x l") 'linum-mode)

;; iEdit mode
(global-set-key (kbd "C-x ;") 'iedit-mode)

;; Web browser (eww)
(global-set-key (kbd "C-x M-w") 'eww)

;; Disable auto fill mode because it's annoying
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; Shortcuts for going forward and backwards cycling windows
(global-set-key (kbd "C-x p") 'other-window)
(global-set-key (kbd "C-x o") 'previous-multiframe-window)

;; Expand region
(use-package expand-region
  :config
  (global-set-key (kbd "\C-x\ =") 'er/expand-region))

;; Avy
(use-package avy
  :config
  (global-set-key (kbd "C-c SPC") 'avy-goto-char))

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
  (add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
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

(use-package emacsql-sqlite3)

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
  :after (org helm)
  :bind (:map org-mode-map
              ("M-." . org-open-at-point)
              ("M-," . org-mark-ring-goto))
  :bind  (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n g" . org-roam-graph)
          ("C-c n c" . org-roam-capture)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n j" . org-roam-dailies-capture-today)
          ("C-c n r" . org-roam-random-note)
          ("C-c n u" . org-roam-unlinked-references)
          ("C-c n e" . org-roam-to-hugo-md)
          ;; Full text search notes with an action to insert
          ;; org-mode link
          ("C-c n s" . helm-rg))

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
                               'my/note-taking-init)

                   (advice-add 'org-roam-capture
                               :after
                               'my/org-roam-capture-set-file-name)

                   (advice-add 'org-roam-dailies-capture-today
                               :after
                               'my/note-taking-init)

                   (advice-add 'org-roam-dailies-capture-today
                               :after
                               'my/org-roam-capture-set-file-name)

                   ;; (advice-add 'org-roam-node-visit
                   ;;             :after
                   ;;             'my/note-taking-init)
                   ))
   org-roam-capture-new-node-hook . (lambda () (my/note-taking-init)))

  :init
  (setq org-roam-directory org-roam-notes-path)
  ;; Needed to supress update warning
  (setq org-roam-v2-ack t)
  ;; Fix Emacs 28.2 can't find `sqlite`
  (setq org-roam-database-connector 'sqlite3)
  ;; Use efm-langserver for prose linting
  (add-hook 'org-roam-mode #'eglot-ensure)
  (add-hook 'org-roam-capture-new-node-hook #'eglot-ensure)

  (setq org-roam-dailies-directory org-roam-notes-path)
  ;; Fix helm results wrapping when there are tags
  ;; https://github.com/org-roam/org-roam/issues/1640
  (require 'helm-mode)
  (add-to-list 'helm-completing-read-handlers-alist
               '(org-roam-node-find . helm-completing-read-sync-default-handler))
  ;; Include tags in note search results
  (setq org-roam-node-display-template "${title}      ${tags}")

  ;; Custom helm source for searching notes based on
  ;; https://ag91.github.io/blog/2022/02/05/an-helm-source-for-org-roam-v2/
  (defun helm-org-roam (&optional input candidates)
    (interactive)
    (require 'org-roam)
    (helm
     :input input
     :sources (list
               (helm-build-sync-source "Find note: "
                 :must-match nil
                 :fuzzy-match t
                 :candidates (or candidates (org-roam-node-read--completions))
                 :persistent-action (lambda (x)
                                      (--> x
                                           (view-file (org-roam-node-file it))))
                 :action
                 '(("Find File" . (lambda (x)
                                    (--> x
                                         (org-roam-node-visit it t))))
                   ("Preview" . (lambda (x)
                                  (--> x
                                       (view-file (org-roam-node-file it)))))
                   ("Insert link" . (lambda (x)
                                      (--> x
                                           (insert
                                            (format
                                             "[[id:%s][%s]]"
                                             (org-roam-node-id it)
                                             (org-roam-node-title it))))))
                   ("Follow backlinks" . (lambda (x)
                                           (let ((candidates
                                                  (--> x
                                                       org-roam-backlinks-get
                                                       (--map
                                                        (org-roam-node-title
                                                         (org-roam-backlink-source-node it))
                                                        it))))
                                             (helm-org-roam nil (or candidates (list x))))))))
               (helm-build-dummy-source
                   "Create note"
                 :action '(("Capture note" . (lambda (candidate)
                                               (org-roam-capture-
                                                :node (org-roam-node-create :title candidate)
                                                :props '(:finalize find-file)))))))))

  (global-set-key (kbd "C-c n f") 'helm-org-roam)

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
       "SELECT id FROM (SELECT links.source AS id, group_concat(tags.tag) AS alltags FROM links LEFT OUTER JOIN tags ON links.source = tags.node_id WHERE links.type = '\"id\"' AND links.dest = '\"%s\"' GROUP BY links.source) Q WHERE alltags IS NULL OR (','||alltags||',' NOT LIKE '%%%%,\"private\",%%%%' AND ','||alltags||',' NOT LIKE '%%%%,\"draft\",%%%%' AND ','||alltags||',' NOT LIKE '%%%%,\"section\",%%%%')"
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
    (let ((notes (org-roam-db-query "SELECT id, file FROM (SELECT nodes.id, nodes.file, group_concat(tags.tag) AS alltags FROM nodes LEFT OUTER JOIN tags ON nodes.id = tags.node_id GROUP BY nodes.file) WHERE alltags is null or (','||alltags||',' not like '%%,\"private\",%%' and ','||alltags||',' not like '%%,\"draft\",%%')")))
      (-map
       (-lambda ((id file))
         ;; Use temporary buffer to prevent a buffer being opened for
         ;; each note file.
         (with-temp-buffer
           (message "Working on: %s" file)

           (insert-file-contents file)

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
                ("s" "Section" plain
                  "%?"
                  :if-new (file+head
                           "${slug}.org"
                           "#+TITLE: ${title}\n#+FILETAGS: section\n\n")
                  :unnarrowed t)
                ("P" "Project" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--project-${slug}.org\" (current-time) t)"
                          "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+CATEGORY: ${slug}\n#+FILETAGS: private project\n\n")
                 :unnarrowed t)
                ("p" "Project Note" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--project-${slug}.org\" (current-time) t)"
                          "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: private project_note\n\n")
                 :unnarrowed t)
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

;; (use-package which-key
;;   :config
;;   (which-key-mode))

;; Macro for running a function repeatedly in the back ground
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

;; Shortcut to create a new term
(define-key global-map (kbd "C-x M-t") 'new-term)

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

;; Make sure we are using ido mode
(use-package ido
  :config
  ;; (setq ido-everywhere t)    ; Not compatible with helm
  (ido-mode (quote both))
  (setq ido-use-faces t)
  ;; Don't magically search for a file that doesn't exist
  (setq ido-auto-merge-work-directories-length -1)
  ;; Allow spaces in searches
  (add-hook 'ido-make-file-list-hook
            (lambda ()
              (define-key ido-file-dir-completion-map (kbd "SPC") 'self-insert-command)))
  )

;; Use ido inside ido buffer too
(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)

  (setq ido-max-directory-size 100000)

  ;; Use the current window when visiting files and buffers with ido
  (setq ido-default-file-method 'selected-window)
  (setq ido-default-buffer-method 'selected-window)

  ;; Disable minibuffer exit for ido
  (put 'ido-exit-minibuffer 'disabled nil)

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n=> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation ()
    (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

  ;; Use normal up/down keyboard shortcuts
  (defun ido-define-keys ()
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys))

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

;; Helm
(use-package helm
  :config
  ;; Use helm with M-x
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-completion-style 'emacs)
  ;; Enables helm everywhere, not compatible with ido-everywhere!
  (helm-mode))

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
  (global-set-key (kbd "C-x M-n") 'projectile-notes))

;; Use ripgrep with projectile
(use-package projectile-ripgrep
  :config
  (global-set-key (kbd "C-c p s r") 'projectile-ripgrep))

;; Use helm projectile
(use-package helm-projectile
  :config
  (helm-projectile-on)

  ;; Use ripgrep with helm
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  ;; Fix helm projectile when using rg...
  ;; https://github.com/syohex/emacs-helm-ag/issues/283
  (defun helm-projectile-ag (&optional options)
    "Helm version of projectile-ag."
    (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
    (if (require 'helm-ag nil  'noerror)
	(if (projectile-project-p)
	    (let ((helm-ag-command-option options)
                  (current-prefix-arg nil))
	      (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
	  (error "You're not in a project"))
      (error "helm-ag not available"))))

(use-package helm-rg
  :config
  ;; Add actions for inserting org file link from selected match
  (defun insert-org-mode-link-from-helm-result (candidate)
    (interactive)
    (with-helm-current-buffer
      (insert (format "[[file:%s][%s]]"
                      (plist-get candidate :file)
                      ;; Extract the title from the file name
                      (subst-char-in-string
                       ?_ ?\s
                       (first
                        (split-string
                         (first
                          (last
                           (split-string (plist-get candidate :file) "\\-")))
                         "\\.")))))))

  (helm-add-action-to-source "Insert org-mode link"
                             'insert-org-mode-link-from-helm-result
                             helm-rg-process-source))

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
(set-face-attribute 'default t :font "Cascadia Code")
(set-face-attribute 'default nil :height 140)

;; Keyboard shortcut for using a big screen
(setq big-screen nil)

(defun toggle-big-screen ()
  (interactive)
  (if big-screen
      (progn
	(setq big-screen nil)
	(set-face-attribute 'default nil :height 120))
    (progn
      (set-face-attribute 'default nil :height 150)
      (setq big-screen 1))))
(global-set-key (kbd "C-x M-b") 'toggle-big-screen)

(use-package docker-tramp)

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
  (define-key global-map (kbd "C-c m m") #'org-remark-mark)
  (define-key org-remark-mode-map (kbd "C-c m o") #'org-remark-open)
  (define-key org-remark-mode-map (kbd "C-c m n") #'org-remark-view-next)
  (define-key org-remark-mode-map (kbd "C-c m p") #'org-remark-view-prev)
  (define-key org-remark-mode-map (kbd "C-c m r") #'org-remark-remove)
  (org-remark-global-tracking-mode +1))

(use-package org-download
  :config
  (setq-default org-download-image-dir (format "%s/img" org-roam-directory)))

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
                    :repo "alphapapa/org-ql"))

;; Fix an issue where the build did not contain helm-org-ql.el
(use-package helm-org-ql
  :straight (helm-org-ql :type git
                    :host github
                    :repo "alphapapa/org-ql"
                    :files ("helm-org-ql.el"))
  :config
  (define-key global-map (kbd "C-c s") #'helm-org-ql-agenda-files))

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key (or (getenv "OPENAI_API_KEY") "")))

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

;; Experimental ox-notion backend
(add-to-list 'load-path "~/.emacs.d/ox-notion/")
(require 'ox-notion)

;; Store customizations in a separate file
(setq custom-file "~/.emacs.d/.customizations.el")
(load custom-file t)

(provide 'init)
(put 'narrow-to-region 'disabled nil)
