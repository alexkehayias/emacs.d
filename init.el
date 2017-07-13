;; Load cask
;; why do I need to do this????

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Use cask and pallet to manage packages
;; You must have cask installed and on the PATH
;; You must run "cask install" before running emacs
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; On OSX, set the environment to the same as the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Disable visual bell
(setq visible-bell nil)

;; Disable backup files
(setq make-backup-files nil)
;; Disable auto save files
(setq auto-save-default nil)

;; Disable pscroll bars in gui emacs
(scroll-bar-mode -1)

;; Disable the tool bar in gui emacs
(tool-bar-mode -1)

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

;; Fix terminal mode sometimes chokes on stdout by loading patched
;; version of term.el http://debbugs.gnu.org/cgi/bugreport.cgi?bug=13350
;(load-file "~/.emacs.d/term-override.el")

;; term color overide???
(defface term-color-black
  '((t (:foreground "#3f3f3f" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-red
  '((t (:foreground "#cc9393" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-green
  '((t (:foreground "#7f9f7f" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-yellow
  '((t (:foreground "#f0dfaf" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-blue
  '((t (:foreground "#6d85ba" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-magenta
  '((t (:foreground "#dc8cc3" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-cyan
  '((t (:foreground "#93e0e3" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-white
  '((t (:foreground "#dcdccc" :background "#272822")))
  "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

;; ansi-term colors
(setq ansi-term-color-vector
  [term term-color-black term-color-red term-color-green term-color-yellow
    term-color-blue term-color-magenta term-color-cyan term-color-white])

;; Always use Chrome when opening links
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Mode-line
;; (add-to-list 'load-path "~/.emacs.d/vendor/ak-modeline")
;; (require 'ak-modeline)
;; (add-to-list 'load-path "~/.emacs.d/vendor/modeline")
;; (require 'armitp-mode-line)
;; Show the time
(display-time-mode 1)

(require 'powerline)
(powerline-default-theme)

;; Window rotate
(require 'rotate)
(global-set-key (kbd "C-c w r") 'rotate-window)
(global-set-key (kbd "C-c w l") 'rotate-layout)

;; Git using magit
(global-set-key (kbd "C-c g") 'magit-status)
;; Always open the commit edit message in a new window instead of the
;; *magit* window so you can see the diff
(add-to-list 'display-buffer-alist
             '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
                                    ((inhibit-same-window . t)))))
;; Disable built in VC mode for better performance
(setq vc-handled-backends nil)
;; Disable refreshing status for better performance
(setq magit-refresh-status-buffer nil)

;; Flycheck
(require 'flycheck)
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Flow
(require 'flycheck-flow)
(add-hook 'javascript-mode-hook 'flycheck-mode)
(flycheck-add-mode 'javascript-flow 'web-mode)

;; Use local eslint since we have modified it
(defun get-eslint-executable ()
  (let ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules")))
    (and root
         (expand-file-name "node_modules/eslint/bin/eslint.js"
                           root))))

(defun my/use-eslint-from-node-modules ()
  (let ((eslint (get-eslint-executable)))
    (when (and eslint (file-executable-p eslint))
      (setq flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Flow and eslint
(flycheck-add-next-checker 'javascript-flow '(t . javascript-eslint))
(global-flycheck-mode)
(global-set-key (kbd "C-c M-n") 'flycheck-next-error)
(global-set-key (kbd "C-c M-p") 'flycheck-previous-error)

;; Rust
;; Use flycheck
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; Use cargo minor mode
(add-hook 'rust-mode-hook 'cargo-minor-mode)
;; Auto format code
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; Rust racer for code completion and jump to definition
(setq racer-rust-src-path "~/Desktop/rustc-1.13.0/src/")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Elisp
(add-hook 'elisp-mode-hook 'paredit-mode)

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Use Pyflakes
;; (require 'pyflakes)

;; Elpy
(elpy-enable)
(setq elpy-rpc-backend "rope")

;; Ruby

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Sane indenting
(setq ruby-deep-indent-paren nil)

;; Ruby repl
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; RVM
;; (global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; Code completion and docs
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Don't auto add file encodings!!!
(setq ruby-insert-encoding-magic-comment nil)

;; Play nice with RVM
;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   (rvm-activate-corresponding-ruby)
;; )

(require 'highlight-indentation)
(add-hook 'enh-ruby-mode-hook
	  (lambda () (highlight-indentation-current-column-mode)))

;; Format line numbers nicely
(setq linum-format (quote " %3d "))

;; Sass-mode
(add-to-list 'load-path "~/.emacs.d/packages/haml-mode")
(require 'haml-mode)
(add-to-list 'load-path "~/.emacs.d/packages/sass-mode")
(require 'sass-mode)
(setq sass-tab-width 2)

;; coffee-mode should use 2 spaces for tabs
(setq coffee-tab-width 2)
;; Turn off auto saving for coffee mode since grunt messes up
(add-hook 'coffee-mode-hook '(lambda () (setq auto-save-default nil)))

;; Processing mode
(add-to-list 'load-path "~/.emacs.d/packages/processing-mode")
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "~/Library/Processing")

;; Jade and Stylus modes
(add-to-list 'load-path "~/.emacs.d/packages/jade-mode")
(add-to-list 'load-path "~/.emacs.d/packages/sws-mode")
(add-to-list 'load-path "~/.emacs.d/packages/stylus-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Octave
;; Use octave-mode for .m files
(autoload 'octave-mode "octave-mod" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Linum mode shortcut
(global-set-key (kbd "C-x l") 'linum-mode)

;; iEdit mode
(global-set-key (kbd "C-x ;") 'iedit-mode)

;; Web browser (eww)
(global-set-key (kbd "C-x M-w") 'eww)

;; Disable auto fill mode because it's fucking annoying
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; Auto-complete company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; Shortcuts for going forward and backwards cycling windows
(global-set-key (kbd "C-x p") 'other-window)
(global-set-key (kbd "C-x o") 'previous-multiframe-window)

;; Expand region
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(require 'expand-region)
(global-set-key (kbd "\C-x\ =") 'er/expand-region)

;; Ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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

;; Kibit for Clojure
;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
   Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

;; Rainbow parantheses
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Cider settings
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

;; Flyspell mode
(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'coffee-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'hbs-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'org-mode-hook (lambda () (flyspell-prog-mode)))

;; Web mode
(require 'web-mode)
(defun web-mode-customization ()
  "Customization for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-coloriztion t))
(add-hook 'web-mode-hook 'web-mode-customization)
;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; Turn off auto saving because js build tools hate temp files
(add-hook 'web-mode-hook '(lambda () (setq auto-save-default nil)))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))

;; Org-mode
(require 'org)

;; When opening a file make sure everything is expanded
(setq org-startup-folded nil)

;; Show inline images
(org-display-inline-images t t)

;; Template blocks for use with org
(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
              ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
              ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
              ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
              ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
              ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+html: " "<literal style=\"html\">?</literal>")
              ("a" "#+begin_ascii\n?\n#+end_ascii")
              ("A" "#+ascii: ")
              ("i" "#+index: ?" "#+index: ?")
              ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))

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
(add-hook 'after-init-hook 'org-agenda-and-todos-two-weeks)

;; Show org timestamps in 12h time
(setq org-agenda-timegrid-use-ampm 1)

;; Always show the current day in org agenda
(setq org-agenda-time-grid '((daily today today)
                             #("----------------" 0 16 (org-heading t))
                             (800 1000 1200 1400 1600 1800 2000)))

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

;; Python virtualenv
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
;; support
(setq venv-location "~/Projects/VideoVentures/connectr")

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
        "~/Dropbox/org/notes.org_archive"))
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c C-a") 'org-agenda)

;; In org agenda log view also show recurring tasks
(setq org-agenda-log-mode-items '(closed clock state))

;; Keyboard shortcut to get to the main todo file
;; (global-set-key (kbd "C-c T")
;;                 (lambda ()
;;                   (interactive)
;;                   (find-file "~/DropBox/org/personal.org")))

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
(setq org-default-notes-file "~/Dropbox/org/refile.org")
;; Allow the creation of parent headings when refiling
(setq org-refile-allow-creating-parent-nodes t)
(setq org-capture-templates
      (quote (("t" "To Do" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U" :clock-in t :clock-resume t)
              ("n" "Note" entry (file "~/Dropbox/org/refile.org")
               "* %? %T :note:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
               "* Meeting w/%? %T :meeting:\n%U" :clock-in t :clock-resume t)
              ("i" "Interview" entry (file "~/Dropbox/org/refile.org")
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

;; Org export reveal
(require 'htmlize)
(require 'ox-reveal)
;; Org export to jira markup
(require 'ox-jira)

;; Hyper org-mode to synchronize org files
;; (add-to-list 'load-path "~/.emacs.d/vendor/hyper-org-mode")
;; (require 'hyper-org-mode)
;; (setq hyper-org-dir "/.hyper-org")
;; (setq hyper-org-url "http://127.0.0.1:1986")
;; (setq hyper-org-files (list "todo.org"))
;; (setq hyper-org-sync-freq-sec 5)

;; Org calendar
;; FIX does not work with cask/pallet
;; (require 'calfw-org)
;; (global-set-key (kbd "C-c C") 'cfw:open-org-calendar)

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
(require 'golden-ratio)
;; (golden-ratio-mode 1)
;; (setq golden-ratio-auto-scale nil)
(defadvice previous-multiframe-window
    (after golden-ratio-resize-window)
  (golden-ratio) nil)

;; Rope
;; (defun load-ropemacs ()
;;   "Load pymacs and ropemacs"
;;   (interactive)
;;   ;; Don't overwrite existing key bindings
;;   (setq ropemacs-enable-shortcuts nil)
;;   (setq ropemacs-local-prefix "C-c C-r")
;;   (require 'pymacs)
;;   (pymacs-load "ropemacs" "rope-")
;;   ;; Automatically save project python buffers before refactorings
;;   (setq ropemacs-confirm-saving 'nil))

;; Rename ansi-terms
(defun new-term (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))

;; Shortcut to create a new term
(define-key global-map (kbd "C-x M-t") 'new-term)

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
  (if centered
    (progn (center-text-clear)
           (setq centered nil))
    (progn (center-text)
           (setq centered t))))

(define-key global-map (kbd "C-c M-t") 'center-text-mode)

;; sexp mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

(setq mac-emulate-three-button-mouse t)

;; Enable system clipboard
(setq x-select-enable-clipboard t)

;; Stackoverflow plugin
(require 'sos)

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

;; ido

;; Make sure we are using ido mode
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-everywhere t)
(ido-mode (quote both))
(setq ido-use-faces t)
;; Don't magically search for a file that doesn't exist
(setq ido-auto-merge-work-directories-length -1)

;; Use ido inside ido buffer too
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)

(setq ido-max-directory-size 100000)

;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; Disable minibuffer exit for ido
(put 'ido-exit-minibuffer 'disabled nil)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n====> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; Use normal up/down keyboard shortcuts
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; Shells should have color
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Json mode
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'json-mode 'flymake-json-load)

;; Handlebars mode
(require 'handlebars-mode)
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . handlebars-mode))

;; Fix pasting from a buffer to ansi-term

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

;; Keyfrequency analysis
(require 'keyfreq)
(setq keyfreq-excluded-commnands
      '(self-insert-command
        abort-recursive-edit
        forward-char
        backward-char
        previous-line
        next-line))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


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

;; Projectile
(projectile-global-mode)
;; Cache projectile projects
(setq projectile-enable-caching t)
;; Defer to git if possible
(setq projectile-indexing-method 'alien)
;; Use ripgrep with projectile
(require 'projectile-ripgrep)
(global-set-key (kbd "C-c p s r") 'projectile-ripgrep)

;; Use helm projectile
(require 'helm-projectile)
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

(setq projectile-enable-caching t)

;; Use git, find, etc
(setq projectile-indexing-method 'alien)

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
    (error "helm-ag not available")))

;; browse-kill-ring with M-y
(browse-kill-ring-default-keybindings)
;; Create a sensible ansi-term
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(defun projectile-term ()
  "Create an ansi-term at the project root"
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
        (ansi-term (getenv "SHELL"))
        (rename-buffer buff-name t)))))

(global-set-key (kbd "C-x M-t") 'projectile-term)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(org-batch-store-agenda-views)

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/replace-colorthemes")

;; Default to molokai theme
(load "~/.emacs.d/themes/color-theme-molokai.el")
(color-theme-molokai)

;; Load custom variables and faces
(setq custom-file "~/.emacs.d/customizations.el")
(load-file custom-file)

;; More pronounced cursor
(set-cursor-color "strawberry")

;; Use doom theme
(require 'doom-themes)
(load-theme 'doom-one t)

;; brighter source buffers
(add-hook 'find-file-hook 'doom-buffer-mode)
;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
;; Fix up some org-mode weirdness with fonts
(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(custom-set-faces
 '(mode-line ((t (:box (:line-width 6 :color "")))))
 '(mode-line-inactive ((t (:box (:line-width 6 :color ""))))))

;; Set default font
;; When using gui emacs with emacsclient, the following makes sure the
;; window is opened with the correct font size otherwise it will
;; revert to 13px for font size
(add-to-list 'default-frame-alist '(font . "Monaco"))
;; Make the default face the same font
(set-face-attribute 'default t :font "Monaco")
(set-face-attribute 'default nil :height 120)
;; Font for all unicode characters
(set-fontset-font t 'unicode "Font Awesome" nil 'prepend)

;; Keyboard shortcut for using a big screen
(setq big-screen nil)

(defun toggle-big-screen ()
  (interactive)
  (if big-screen
      (progn
	(setq big-screen nil)
	(set-face-attribute 'default nil :height 120))
    (progn
      (set-face-attribute 'default nil :height 160)
      (setq big-screen 1))))
(global-set-key (kbd "C-x M-b") 'toggle-big-screen)

;; Up the threshold for garbage collection
(setq gc-cons-threshold 10000000)

;; Replace char symbols with unicode characters
(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
		      ;; arrows
		      ('left-arrow 8592)
		      ('up-arrow 8593)
		      ('right-arrow 8594)
		      ('down-arrow 8595)
		      ;; boxes
		      ('double-vertical-bar #X2551)
		      ;; relational operators
		      ('equal #X003d)
		      ('not-equal #X2260)
		      ('identical #X2261)
		      ('not-identical #X2262)
		      ('less-than #X003c)
		      ('greater-than #X003e)
		      ('less-than-or-equal-to #X2264)
		      ('greater-than-or-equal-to #X2265)
		      ;; logical operators
		      ('logical-and #X2227)
		      ('logical-or #X2228)
		      ('logical-neg #X00AC)
		      ;; misc
		      ('nil #X2205)
		      ('horizontal-ellipsis #X2026)
		      ('double-exclamation #X203C)
		      ('prime #X2032)
		      ('double-prime #X2033)
		      ('for-all #X2200)
		      ('there-exists #X2203)
		      ('element-of #X2208)
		      ;; mathematical operators
		      ('square-root #X221A)
		      ('squared #X00B2)
		      ('cubed #X00B3)
		      ;; letters
		      ('lambda #X03BB)
		      ('alpha #X03B1)
		      ('beta #X03B2)
		      ('gamma #X03B3)
		      ('delta #X03B4))))

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
					     ,(unicode-symbol symbol))
			     nil))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
	      (substitute-pattern-with-unicode (car x)
					       (cdr x)))
	  patterns))

(defun rust-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "\\(<-\\)" 'left-arrow)
	 (cons "\\(->\\)" 'right-arrow)
	 (cons "\\[^=\\]\\(=\\)\\[^=\\]" 'equal)
	 (cons "\\(==\\)" 'identical)
	 (cons "\\(\\!=\\)" 'not-identical)
	 (cons "\\<\\(sqrt\\)\\>" 'square-root)
	 (cons "\\<\\(not\\)\\>" 'logical-neg)
	 (cons "\\(>\\)\\[^=\\]" 'greater-than)
	 (cons "\\(<\\)\\[^=\\]" 'less-than)
	 (cons "\\(>=\\)" 'greater-than-or-equal-to)
	 (cons "\\(<=\\)" 'less-than-or-equal-to))))

(add-hook 'rust-mode-hook 'rust-unicode)

(provide 'init)

(fset 'big-emoji
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (":big-:" 0 "%d")) arg)))

(fset 'jira-epic
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("jira create -p ATLASENG -i Epic -t epic --noedit -o epicname=\",\" -o labels=\"\306\" -o summary=\"\"" 0 "%d")) arg)))
