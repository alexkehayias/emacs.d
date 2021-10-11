;; Initialize straight.el and use-package.el
;; Assumes every use-package uses straight
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


(setq completion-styles '(flex basic partial-completion emacs22))

;; Copy file name to clipboard
(defun copy-file-name ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (x-select-text filename))))

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
  ;; Always open the commit edit message in a new window instead of the
  ;; *magit* window so you can see the diff
  (add-to-list 'display-buffer-alist
	       '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
				      ((inhibit-same-window . t))))))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;; Flycheck
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-global-modes '((not rust-mode)
                                (not python-mode)))
  (global-set-key (kbd "C-c M-n") 'flycheck-next-error)
  (global-set-key (kbd "C-c M-p") 'flycheck-previous-error)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

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

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
  (add-hook 'typescript-mode-hook #'eglot-ensure))

(use-package toml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode)))

(use-package rust-mode
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook #'eglot-ensure))

(use-package eglot
  :config
  (global-set-key (kbd "M-n") 'flymake-goto-next-error)
  (global-set-key (kbd "M-p") 'flymake-goto-prev-error)

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

  (setq-default eglot-workspace-configuration
        '((:pyls . ((:plugins .
                              ((:pycodestyle . ((:enabled . :json-false)))
                               (:pyls_black . ((:enabled . t)))))))))

  (defun my-project-try-pyproject-toml (dir)
    (when-let* ((found (locate-dominating-file dir "pyproject.toml")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions 'my-project-try-pyproject-toml nil nil)

  (add-to-list 'eglot-server-programs
               '((python-mode) "/Users/alex/mosey/app/.docker-python-language-server"))
  )

;; HACK: If the xref file doesn't exist, it probably came from a
;; remote LSP server using eglot. Try mapping it to the local
;; file system. Maybe someday it will be supported in eglot.
;; See: https://github.com/joaotavora/eglot/issues/350
(eval-after-load "xref"
  '(defun xref-make-file-location (file line column)
     (if (not (file-exists-p file))
         (make-instance 'xref-file-location :file (format "~/mosey%s" file) :line line :column column)
       (make-instance 'xref-file-location :file file :line line :column column))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

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
  (add-hook 'python-mode-hook #'eglot-ensure))

;; Ruby
(use-package robe
  :config
  ;; Sane indenting
  (setq ruby-deep-indent-paren nil)
  ;; Don't auto add file encodings!!!
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook 'robe-mode)
  ;; Use specific rubocop
  (defun use-custom-rubocop ()
    (let* ((root (locate-dominating-file
		  (or (buffer-file-name) default-directory)
		  "scripts/bin/rubocop"))
	   (rubocop (and root
			 (expand-file-name "scripts/bin/rubocop"
					   root))))
      (when (and rubocop (file-executable-p rubocop))
	(setq-local flycheck-ruby-rubocop-executable rubocop))))
  (add-hook 'flycheck-mode-hook #'use-custom-rubocop))

;; Format line numbers nicely
(setq linum-format (quote " %3d "))

;; Sass-mode
(use-package sass-mode
  :config (setq sass-tab-width 2))

;; Processing mode
(use-package processing-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
  (setq processing-location "~/Library/Processing"))

;; Markdown
(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    ))

(global-set-key (kbd "C-c s") 'toggle-line-spacing)

(defun writeroom-setup ()
  (interactive)
  ;; Use custom font face for this buffer only
  (defface tmp-buffer-local-face
    '((t :family "Space Mono" :height 220))
    "Temporary buffer-local face")
  (buffer-face-set 'tmp-buffer-local-face)
  ;; Use a skinny cursor
  (make-local-variable 'cursor-type)
  (setq cursor-type 'bar))

;; Nice writing layout
(use-package writeroom-mode
  :config
  (add-hook 'writeroom-mode-hook 'writeroom-setup))

;; Detect things like weasel words
(use-package writegood-mode
  :config
  (global-set-key "\C-c\C-gl" 'writegood-grade-level)
  (global-set-key "\C-c\C-ge" 'writegood-reading-ease))

(use-package flymake-proselint
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (flymake-mode +1)
                             (flymake-proselint-setup))))

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

;; Auto-complete company-mode
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-capf))

;; Shortcuts for going forward and backwards cycling windows
(global-set-key (kbd "C-x p") 'other-window)
(global-set-key (kbd "C-x o") 'previous-multiframe-window)

;; Expand region
(use-package expand-region
  :config
  (global-set-key (kbd "\C-x\ =") 'er/expand-region))

;; Ace jump mode
(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode))

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
  (add-hook 'org-mode-hook (lambda () (flyspell-prog-mode))))

;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; Org-mode

;; Copied from https://github.com/glasserc/etc/commit/3af96f2c780a35d35bdf1b9ac19d80fe2e6ebbf8
;; Work around built-in org-mode so we can load from ELPA.
;; First, remove the built-in org directory from the load-path.
;; Thanks to
;; http://stackoverflow.com/questions/20603578/emacs-does-not-see-new-installation-of-org-mode/20616703#20616703.
;; Without this, use-package will try to require org and succeed.
(eval-when-compile
  (require 'cl))

(setq load-path
      (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
;; Second, trick emacs into forgetting about the fact that org is
;; a "built-in" package by removing it from package--builtins.
;; Without this, package will refuse to install org, since it's
;; "already installed".
;; package--builtins is only initialized when a query needs it.
(package-built-in-p 'org)   ;; prime package--builtins
(setq package--builtins (assq-delete-all 'org package--builtins))

(use-package org
  :config
  (setq org-directory "~/Org")

  ;; When opening a file make sure everything is expanded
  (setq org-startup-folded nil)

  ;; Always wrap lines
  (setq org-startup-truncated nil)

  ;; Hide markers like /emphasis/
  (setq org-hide-emphasis-markers t)

  ;; Show inline images
  (org-display-inline-images t t)

  ;; Don't show full size images otherwise it's too large when
  ;; displaying inline
  (setq org-image-actual-width nil)

  ;; Refile to the root of a file
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((org-agenda-files :level . 1)))

  ;; Show the agenda helper and viewer in split screen
  (defadvice org-agenda (around split-vertically activate)
    (let ((split-width-threshold 80))
      ad-do-it))

  (defadvice org-agenda-list (around split-vertically activate)
    (let ((split-width-threshold 80))
      ad-do-it))

  (defun org-agenda-and-todos ()
    (interactive)
    (org-agenda nil "n"))

  (defun org-agenda-and-todos-two-weeks ()
    (interactive)
    (let ((current-prefix-arg 14))
      (call-interactively 'org-agenda-and-todos)))

  (defun org-agenda-and-todos-last-week ()
    (interactive)
    (let ((current-prefix-arg -7))
      (call-interactively 'org-agenda-and-todos)))

  ;; Don't export headings with numbers
  ;; (setq org-export-with-section-numbers nil)

  ;; gnuplot
  (local-set-key (kbd "M-C-g") 'org-plot/gnuplot)

  ;; Shortcut copy an internal link
  (global-set-key (kbd "C-c l") 'org-store-link)

  ;; Shortcut to show preferred agenda view
  (global-set-key (kbd "C-c A") 'org-agenda-and-todos-two-weeks)

  ;; Show hours:minutes instead of days:hours
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

  ;; Shortcut to jump to last running clock
  (global-set-key (kbd "C-c C-x C-j") 'org-clock-jump-to-current-clock)

  ;; On startup show the agenda for the next 2 calendar weeks and all
  ;; todo items
  ;; (add-hook 'after-init-hook 'org-agenda-and-todos-two-weeks)

  ;; Show org timestamps in 12h time
  (setq org-agenda-timegrid-use-ampm 1)

  ;; Always show the current day in org agenda
  (setq org-agenda-time-grid (quote
                              ((daily today today)
                               (800 1000 1200 1400 1600 1800 2000)
                               "......" "----------------")))

  ;; Color code the agenda based on type
  ;; http://dept.stat.lsa.umich.edu/~jerrick/org_agenda_calendar.html
  (defun color-org-header (tag col)
    (interactive)
    (goto-char (point-min))
    (while (re-search-forward tag nil t)
      (add-text-properties (match-beginning 0) (point-at-eol)
			   `(face (:foreground ,col)))))

  (add-hook 'org-finalize-agenda-hook
	    (lambda ()
	      (save-excursion
		(color-org-header "notes:"  "#66D9EF")
		(color-org-header "refile:" "#F92672"))))

  ;; Use longtable as the default table style when exporting
  (setq org-latex-default-table-environment "longtable")

  ;; Don't position tables in the center
  (setq org-latex-tables-centered nil)

  ;; Fix some org-mode + yasnippet conflicts
  ;; http://stackoverflow.com/questions/9418148/conflicts-between-org-mode-and-yasnippet
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (make-variable-buffer-local 'yas/trigger-key)
	      (setq yas/trigger-key [tab])
	      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	      (define-key yas/keymap [tab] 'yas/next-field)))

  (add-hook 'org-mode-hook (lambda ()
                             (make-variable-buffer-local 'visual-line-mode)
                             (visual-line-mode)))

  ;; Show syntax highlighting per language native mode in *.org
  (setq org-src-fontify-natively t)
  ;; For languages with significant whitespace like Python:
  (setq org-src-preserve-indentation t)

  ;; Timestamp new todos
  (setq org-log-done 'time)
  (setq org-startup-indented t)

  ;; Agenda
  (setq org-agenda-text-search-extra-files
	'(agenda-archives
	  "~/Org/notes.org_archive"))
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c C-a") 'org-agenda)

  ;; In org agenda log view also show recurring tasks
  (setq org-agenda-log-mode-items '(closed clock state))

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
  (setq org-default-notes-file "~/Org/refile.org")
  ;; Allow the creation of parent headings when refiling
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-capture-templates
	(quote (("t" "To Do" entry (file "~/Org/refile.org")
		 "* TODO %?\n%U" :clock-in t :clock-resume t)
		("n" "Note" entry (file "~/Org/refile.org")
		 "* %? %T :note:\n%U\n%a\n" :clock-in t :clock-resume t)
		("m" "Meeting" entry (file "~/Org/refile.org")
		 "* Meeting w/%? %T :meeting:\n%U" :clock-in t :clock-resume t)
		("i" "Interview" entry (file "~/Org/refile.org")
		 "* Interview w/%? %T :interview:\n%U" :clock-in t :clock-resume t))))

  ;; Auto mark parent todos as done if childrend are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;; Refile

					; Targets include this file and any file contributing to the agenda -
                                        ; up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel . 9))))

					; Use full outline paths for refile targets - we file directly with
                                        ; IDO
  (setq org-refile-use-outline-path t)

                                        ; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

                                        ; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

                                        ; Use IDO for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)
                                        ; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Time format for clock table durations as h:mm
  (setq org-duration-format (quote h:mm))

  ;; Don't prompt for confirmation when exporting babel blocks
  (setq org-confirm-babel-evaluate nil))

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
  :after ox
  :config
  (defun org-hugo-link (link desc info)
    ;; HACK: Override ox-hugo export of org-id links to make them relrefs
    "Convert LINK to Markdown format.

     DESC is the link's description.
     INFO is a plist used as a communication channel.

     Unlike `org-md-link', this function will also copy local images
     and rewrite link paths to make blogging more seamless."
    (let* ((raw-link (org-element-property :raw-link link))
           (raw-path (org-element-property :path link))
           (type (org-element-property :type link))
           (link-is-url (member type '("http" "https" "ftp" "mailto"))))
      (when (and (stringp raw-path)
                 link-is-url)
        (setq raw-path (org-blackfriday--url-sanitize
                        (url-encode-url raw-path))))
      ;; (message "[ox-hugo-link DBG] link: %S" link)
      ;; (message "[ox-hugo-link DBG] link path: %s" (org-element-property :path link))
      ;; (message "[ox-hugo-link DBG] link filename: %s" (expand-file-name (plist-get (car (cdr link)) :path)))
      (message "[ox-hugo-link DBG] link type: %s" type)
      (cond
       ;; Link type is handled by a special function.
       ((org-export-custom-protocol-maybe link desc 'md))
       ((member type '("custom-id" "fuzzy"))
        (let ((destination (if (string= type "fuzzy")
                               (org-export-resolve-fuzzy-link link info)
                             (org-export-resolve-id-link link info))))
          (message "[org-hugo-link DBG] link destination elem type: %S" (org-element-type destination))
          (pcase (org-element-type destination)
            (`plain-text                  ;External file
             (let ((path (progn
                           ;; Treat links to `file.org' as links to `file.md'.
                           (if (string= ".org" (downcase (file-name-extension destination ".")))
                               (concat (file-name-sans-extension destination) ".md")
                             destination))))
               (if desc
                   (format "[%s](%s)" desc path)
                 (format "<%s>" path))))
            (`headline                 ;Links of type [[* Some heading]]
             (let ((title (org-export-data (org-element-property :title destination) info)))
               (format
                "[%s](#%s)"
                ;; Description
                (cond ((org-string-nw-p desc))
                      ((org-export-numbered-headline-p destination info)
                       (mapconcat #'number-to-string
                                  (org-export-get-headline-number destination info)
                                  "."))
                      (t
                       title))
                ;; Reference
                (org-hugo--get-anchor destination info title))))
            (_
             (let ((description
                    (or (org-string-nw-p desc)
                        (let ((number (org-export-get-ordinal
                                       destination info
                                       nil #'org-html--has-caption-p)))
                          (when number
                            (let ((num-str (if (atom number)
                                               (number-to-string number)
                                             (mapconcat #'number-to-string number "."))))
                              ;; (message "[ox-hugo-link DBG] num-str: %s" num-str)
                              (if org-hugo-link-desc-insert-type
                                  (let* ((type (org-element-type destination))
                                         ;; Org doesn't have a specific
                                         ;; element for figures. So if
                                         ;; the element is `paragraph',
                                         ;; and as this element has an
                                         ;; ordinal, we will assume that
                                         ;; to be a figure.
                                         (type (if (equal 'paragraph type)
                                                   'figure
                                                 type))
                                         (type-str (org-blackfriday--translate type info)))
                                    (format "%s %s" type-str num-str))
                                num-str)))))))
               ;; (message "[ox-hugo-link DBG] link description: %s" description)
               (when description
                 (format "[%s](#%s)"
                         description
                         (if (memq (org-element-type destination) '(src-block table))
                             (org-blackfriday--get-reference destination)
                           (org-export-get-reference destination info)))))))))
       ((org-export-inline-image-p link org-html-inline-image-rules)
        ;; (message "[org-hugo-link DBG] processing an image: %s" desc)
        (let* ((parent (org-export-get-parent link))
               (parent-type (org-element-type parent))
               ;; If this is a hyper-linked image, it's parent type will
               ;; be a link too. Get the parent of *that* link in that
               ;; case.
               (grand-parent (when (eq parent-type 'link)
                               (org-export-get-parent parent)))
               (useful-parent (if grand-parent
                                  grand-parent
                                parent))
               (attr (org-export-read-attribute :attr_html useful-parent))
               (caption (or
                         ;; Caption set using #+caption takes higher precedence.
                         (org-string-nw-p
                          (org-export-data  ;Look for caption set using #+caption
                           (org-export-get-caption (org-export-get-parent-element link))
                           info))
                         (plist-get attr :caption)))
               (caption (when (org-string-nw-p caption)
                          (format "%s%s%s%s"
                                  ;; Tue Feb 13 11:32:45 EST 2018 - kmodi
                                  ;; Add the span tag once
                                  ;; https://github.com/gohugoio/hugo/issues/4406
                                  ;; gets resolved.
                                  "" ;"<span class=\\\"figure-number\\\">"
                                  (format (org-html--translate
                                           (concat
                                            (cdr (assoc 'figure org-blackfriday--org-element-string))
                                            " %d:")
                                           info)
                                          (org-export-get-ordinal
                                           useful-parent info
                                           nil #'org-html--has-caption-p))
                                  " "     ;" </span>"
                                  ;; Escape the double-quotes, if any.
                                  (replace-regexp-in-string "\"" "\\\\\"" caption))))
               (extension (downcase (file-name-extension raw-path)))
               (inlined-svg (and (stringp extension)
                                 (string= "svg" extension)
                                 (plist-get attr :inlined))))
          ;; (message "[ox-hugo-link DBG] Inline image: %s, extension: %s" raw-path extension)
          ;; (message "[ox-hugo-link DBG] inlined svg? %S" inlined-svg)
          ;; (message "[ox-hugo-link DBG] caption: %s" caption)
          (if inlined-svg
              (let* ((svg-contents (with-temp-buffer
                                     (insert-file-contents raw-path)
                                     (fill-region (point-min) (point-max)) ;Make huge one-liner SVGs sane
                                     (buffer-substring-no-properties (point-min) (point-max))))
                     (svg-contents-sanitized (replace-regexp-in-string
                                              ;; Remove the HTML comments.
                                              "<!--\\(.\\|\n\\)*?-->" ""
                                              (replace-regexp-in-string
                                               ;; Remove the xml document tag as that cannot be inlined in-between
                                               ;; a Markdown (or even an HTML) file.
                                               "<\\?xml version=\"1\\.0\" encoding=\"UTF-8\" standalone=\"no\"\\?>" ""
                                               svg-contents)))
                     (caption-html (if (not caption)
                                       ""
                                     (format (concat "\n\n<div class=\"figure-caption\">\n"
                                                     "  %s\n"
                                                     "</div>")
                                             (org-html-convert-special-strings ;Interpret em-dash, en-dash, etc.
                                              (org-export-data-with-backend caption 'html info))))))
                ;; (message "[ox-hugo-link DBG] svg contents: %s" svg-contents)
                ;; (message "[ox-hugo-link DBG] svg contents sanitized: %s" svg-contents-sanitized)
                (concat svg-contents-sanitized caption-html))
            (let* ((path (org-hugo--attachment-rewrite-maybe raw-path info))
                   (inline-image (not (org-html-standalone-image-p useful-parent info)))
                   (source (if link-is-url
                               (concat type ":" path)
                             path))
                   (num-attr (/ (length attr) 2)) ;(:alt foo) -> num-attr = 1
                   (alt-text (plist-get attr :alt)))
              ;; (message "[ox-hugo-link DBG] path: %s" path)
              ;; (message "[ox-hugo-link DBG] inline image? %s" inline-image)
              ;; (message "[org-hugo-link DBG] attr: %s num of attr: %d"
              ;;          attr (length attr))
              ;; (message "[org-hugo-link DBG] parent-type: %s" parent-type)
              ;; (message "[org-hugo-link DBG] useful-parent-type: %s"
              ;;          (org-element-type useful-parent))
              (cond
               (;; Use the Markdown image syntax if the image is inline and
                ;; there are no HTML attributes for the image, or just one
                ;; attribute, the `alt-text'.
                (and inline-image
                     (or (= 0 num-attr)
                         (and alt-text
                              (= 1 num-attr))))
                (let ((alt-text (if alt-text
                                    alt-text
                                  "")))
                  (format "![%s](%s)" alt-text source)))
               (;; Else if the image is inline (with non-alt-text
                ;; attributes), use HTML <img> tag syntax.
                inline-image
                ;; The "target" and "rel" attributes would be meant for <a>
                ;; tags. So do not pass them to the <img> tag.
                (plist-put attr :target nil)
                (plist-put attr :rel nil)
                (org-html--format-image source attr info))
               (t ;Else use the Hugo `figure' shortcode.
                ;; Hugo `figure' shortcode named parameters.
                ;; https://gohugo.io/content-management/shortcodes/#figure
                (let ((figure-params `((src . ,source)
                                       (alt . ,alt-text)
                                       (caption . ,caption)
                                       (link . ,(plist-get attr :link))
                                       (title . ,(plist-get attr :title))
                                       (class . ,(plist-get attr :class))
                                       (attr . ,(plist-get attr :attr))
                                       (attrlink . ,(plist-get attr :attrlink))
                                       (width . ,(plist-get attr :width))
                                       (height . ,(plist-get attr :height))
                                       ;; While the `target' and `rel'
                                       ;; attributes are not supported by
                                       ;; the inbuilt Hugo `figure'
                                       ;; shortcode, they can be used as
                                       ;; intended if a user has a custom
                                       ;; `figure' shortcode with the
                                       ;; support added for those.
                                       (target . ,(plist-get attr :target))
                                       (rel . ,(plist-get attr :rel))))
                      (figure-param-str ""))
                  (dolist (param figure-params)
                    (let ((name (car param))
                          (val (cdr param)))
                      (when val
                        (setq figure-param-str (concat figure-param-str
                                                       (format "%s=\"%s\" "
                                                               name val))))))
                  ;; (message "[org-hugo-link DBG] figure params: %s" figure-param-str)
                  (format "{{< figure %s >}}" (org-trim figure-param-str)))))))))
       ((equal type "id")
        (let ((description (org-string-nw-p desc))
              (path (org-hugo--attachment-rewrite-maybe raw-path info)))
          (format "[%s]({{< relref \"%s\" >}})"
                  description
                  (file-name-sans-extension
                   (file-name-nondirectory (car (org-id-find raw-path)))))))
       ((string= type "coderef")
        (let ((ref (org-element-property :path link)))
          (format (org-export-get-coderef-format ref desc)
                  (org-export-resolve-coderef ref info))))
       ((equal type "radio")
        (let ((destination (org-export-resolve-radio-link link info)))
          (format "[%s](#%s)" desc (org-export-get-reference destination info))))
       (t
        (let* ((link-param-str "")
               (path (cond
                      (link-is-url
                       ;; Taken from ox-html.el -- Extract attributes
                       ;; from parent's paragraph.  HACK: Only do this
                       ;; for the first link in parent (inner image link
                       ;; for inline images).  This is needed as long as
                       ;; attributes cannot be set on a per link basis.
                       (let* ((attr
                               (let ((parent (org-export-get-parent-element link)))
                                 (and (eq (org-element-map parent 'link #'identity info :first-match) link)
                                      (org-export-read-attribute :attr_html parent))))
                              ;; https://www.w3schools.com/tags/tag_link.asp
                              (link-params `((media . ,(plist-get attr :media))
                                             (target . ,(plist-get attr :target))
                                             (rel . ,(plist-get attr :rel))
                                             (sizes . ,(plist-get attr :sizes))
                                             (type . ,(plist-get attr :type)))))
                         (dolist (param link-params)
                           (let ((name (car param))
                                 (val (cdr param)))
                             (when val
                               (setq link-param-str (concat link-param-str
                                                            (format "%s=\"%s\" "
                                                                    name val))))))
                         ;; (message "[ox-hugo-link DBG] link params: %s" link-param-str)
                         )
                       (concat type ":" raw-path))
                      (;; Remove the "file://" prefix.
                       (string= type "file")
                       ;; (message "[ox-hugo-link DBG] raw-path: %s" raw-path)
                       (let ((path1 (replace-regexp-in-string "\\`file://" "" raw-path)))
                         (if (string= ".org" (downcase (file-name-extension path1 ".")))
                             (let ((raw-link-minus-org-file
                                    ;; If raw-link is "./foo.org::#bar",
                                    ;; set `raw-link-minus-org-file' to
                                    ;; "#bar".
                                    (if (string-match ".*\\.org::\\(#.*\\)" raw-link)
                                        (match-string-no-properties 1 raw-link)
                                      "")))
                               (format "{{< relref \"%s%s\" >}}"
                                       (file-name-sans-extension
                                        (file-name-nondirectory path1))
                                       raw-link-minus-org-file))
                           (org-hugo--attachment-rewrite-maybe path1 info))))
                      (t
                       raw-path)))
               (link-param-str (org-string-nw-p (org-trim link-param-str))))
          ;; (message "[ox-hugo-link DBG] desc=%s path=%s" desc path)
          ;; (message "[ox-hugo-link DBG] link-param-str=%s" link-param-str)
          (cond
           ;; Link description is a `figure' shortcode but does not
           ;; already have the `link' parameter set.
           ((and desc
                 (string-match-p "\\`{{<\\s-*figure\\s-+" desc)
                 (not (string-match-p "\\`{{<\\s-*figure\\s-+.*link=" desc)))
            (replace-regexp-in-string "\\s-*>}}\\'"
                                      (format " link=\"%s\"\\&" path)
                                      desc))
           ;; Both link description and link attributes are present.
           ((and desc
                 link-param-str)
            (format "<a href=\"%s\" %s>%s</a>"
                    (org-html-encode-plain-text path)
                    link-param-str
                    (org-link-unescape desc)))
           ;; Only link description, but no link attributes.
           (desc
            (let* ((path-has-space (and
                                    (not (string-prefix-p "{{< relref " path))
                                    (string-match-p "\\s-" path)))
                   (path (if path-has-space
                             ;; https://github.com/kaushalmodi/ox-hugo/issues/376
                             ;; https://github.com/gohugoio/hugo/issues/6742#issuecomment-573924706
                             (format "<%s>" path)
                           path)))
              (format "[%s](%s)" desc path)))
           ;; Only link attributes, but no link description.
           (link-param-str
            (let ((path (org-html-encode-plain-text path)))
              (format "<a href=\"%s\" %s>%s</a>"
                      path
                      link-param-str
                      (org-link-unescape path))))
           ;; Neither link description, nor link attributes.
           (t
            (if (string-prefix-p "{{< relref " path)
                (format "[%s](%s)" path path)
              (format "<%s>" path))))))))))

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

(use-package org-roam
  :after org
  :hook
  ;; Need to add advice after-init otherwise they won't take
  ((after-init . (lambda ()
                   (advice-add 'org-roam-capture
                               :after
                               'my/note-taking-init)

                   (advice-add 'org-roam-dailies-capture-today
                               :after
                               'my/note-taking-init)

                   (advice-add 'org-roam-node-find
                               :after
                               'my/note-taking-init)

                   (advice-add 'org-roam-node-insert
                               :after
                               'my/note-taking-init))))

  :init
  (setq org-roam-directory org-roam-notes-path)
  ;; Needed to supress update warning
  (setq org-roam-v2-ack t)
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
  ;; draft
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
       "SELECT id FROM (SELECT links.source AS id, group_concat(tags.tag) AS alltags FROM links LEFT OUTER JOIN tags ON links.source = tags.node_id WHERE links.type = '\"id\"' AND links.dest = '\"%s\"' GROUP BY links.source) Q WHERE alltags IS NULL OR (','||alltags||',' NOT LIKE '%%%%,\"private\",%%%%' AND ','||alltags||',' NOT LIKE '%%%%,\"draft\",%%%%')"
       id))))

  (defun file-path-to-md-file-name (path)
    (let ((file-name (first (last (split-string path "/")))))
      (concat (first (split-string file-name "\\.")) ".md")))

  (defun file-path-to-slug (path)
    (let* ((file-name (car (last (split-string path "--"))))
           (title (first (split-string file-name "\\."))))
      (replace-regexp-in-string (regexp-quote "_") "-" title nil 'literal)))

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

  :bind  (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
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

  :config
  (defun my/org-id-update-org-roam-files ()
    "Update Org-ID locations for all Org-roam files."
    ;; https://org-roam.discourse.group/t/org-roam-v2-org-id-id-link-resolution-problem/1491/4
    (interactive)
    (org-id-update-id-locations (org-roam--list-all-files)))

  (setq org-roam-capture-templates
	(quote (("d" "Default" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}.org\" (current-time) t)"
                          "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n\n")
                 :unnarrowed t))))

  ;; Journaling setup
  (setq org-roam-dailies-directory "")

  (setq org-roam-dailies-capture-templates
	(quote (("d" "Default" plain
                 "%?"
                 :if-new (file+head
                          "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--journal.org\" (current-time) t)"
                          "#+TITLE: Journal %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: private journal\n\n\n")
                 :unnarrowed t))))

  ;; Use writeroom mode when capturing new notes. Hide the ugly
  ;; preamble of org attributes by scrolling up.
  (defun my/note-taking-init (&rest r)
    (with-current-buffer (current-buffer)
      (writeroom-mode)
      (scroll-up-command 6)))

  (org-roam-db-autosync-mode))


(use-package which-key
  :config
  (which-key-mode))

;; Macro for running a function repeatedly in the back ground
;; https://github.com/punchagan/dot-emacs/blob/master/punchagan.org
(defmacro run-with-timer-when-idle (secs idle-time repeat-time function &rest args)
  "Run a function on timer, but only when idle."
  `(run-with-timer
    ,secs
    ,repeat-time
    (lambda () (run-with-idle-timer ,idle-time nil ,function ,@args))))

;; Use golden ratio mode which automatically resizes buffers based on
;; the golden ratio
(use-package golden-ratio
  :config
  ;; (golden-ratio-mode 1)
  ;; (setq golden-ratio-auto-scale nil)
  (defadvice previous-multiframe-window
      (after golden-ratio-resize-window)
    (golden-ratio) nil))

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

;; Center text
(defun center-text ()
  "Center the text in the middle of the buffer. Works best in full screen"
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                        (/ (window-width) 4)
                        (/ (window-width) 4)))

(defun center-text-clear ()
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                        nil
                        nil))

(setq centered nil)

(defun center-text-mode ()
  (interactive)
  (make-local-variable 'centered)
  (if (eq centered t)
    (progn (center-text-clear)
           (setq centered nil))
    (progn (center-text)
           (setq centered t))))

(define-key global-map (kbd "C-c M-t") 'center-text-mode)

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

;; Growl integration
(defvar growlnotify-command
  (executable-find "growlnotify")
  "/usr/local/bin/growlnotify")

(defun growl (title message)
  "Shows a message through the growl notification system using
 `growlnotify-command` as the program."
  (cl-flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))))
    (let* ((process (start-process "growlnotify" nil
                                   growlnotify-command
                                   (encfn title)
                                   "-a" "Emacs"
                                   "-n" "Emacs")))
      (process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  t)

;; IRC
(add-hook 'erc-join-hook 'bitlbee-identify)
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   bitlbee-password))))

;; Growl notification when mentioned in ERC
(defun erc-growl-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (growl
   (concat "ERC: new message on: " (buffer-name (current-buffer)))
   message)
  ;; (unless (posix-string-match "^\\** *Users on #" message)
  ;;   (growl
  ;;    (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
  ;;    message))
  )

(add-hook 'erc-text-matched-hook 'erc-growl-hook)


;; Helm

(use-package helm
  :config
  ;; Use helm with M-x
  (global-set-key (kbd "M-x") 'helm-M-x)
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

  ;; Speed up helm flx matching
  (defvar helm-ido-like-user-gc-setting nil)

  (defun helm-ido-like-higher-gc ()
    (setq helm-ido-like-user-gc-setting gc-cons-threshold)
    (setq gc-cons-threshold most-positive-fixnum))

  (defun helm-ido-like-lower-gc ()
    (setq gc-cons-threshold helm-ido-like-user-gc-setting))

  (defun helm-ido-like-load-fuzzy-enhancements ()
    (add-hook 'minibuffer-setup-hook #'helm-ido-like-higher-gc)
    (add-hook 'minibuffer-exit-hook #'helm-ido-like-lower-gc))

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

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 40))

(use-package all-the-icons)

;; Set default font
;; When using gui emacs with emacsclient, the following makes sure the
;; window is opened with the correct font size otherwise it will
;; revert to 13px for font size
(add-to-list 'default-frame-alist '(font . "Cascadia Code"))
;; Make the default face the same font
(set-face-attribute 'default t :font "Cascadia Code")
(set-face-attribute 'default nil :height 120)

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

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; Store customizations in a separate file
(setq custom-file "~/.emacs.d/.customizations.el")
(load custom-file t)

(provide 'init)
