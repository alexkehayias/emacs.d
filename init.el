;; Use cask and pallet to manage packages
;; You must have cask installed and on the PATH
;; You must run "cask install" before running emacs
(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; On OSX, set the environment to the same as the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Disable visual bell
(setq visible-bell nil)

;; Disable scroll bars in gui emacs
(scroll-bar-mode -1)

;; Disable the tool bar in gui emacs
(tool-bar-mode -1)

;; Don't suspend emacs with C-z
(global-unset-key [?\C-z])

;; Use iBuffer by default for C-x C-b which is more readable than default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Shortcut for occur
(global-set-key (kbd "C-x C-o") 'occur)

;; Fix terminal mode sometimes chokes on stdout by loading patched
;; version of term.el http://debbugs.gnu.org/cgi/bugreport.cgi?bug=13350
(load-file "~/.emacs.d/term-override.el")

;; Always use Chrome when opening links
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Mode-line
(add-to-list 'load-path "~/.emacs.d/vendor/modeline")
(require 'armitp-mode-line)
;; Show the time
(display-time-mode 1)

;; Git using magit
(global-set-key (kbd "C-c g") 'magit-status)
;; Always open the commit edit message in a new window instead of the
;; *magit* window so you can see the diff
(add-to-list 'display-buffer-alist
             '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
                                    ((inhibit-same-window . t)))))
;; Disable built in VC mode for better performance
(setq vc-handled-backends nil)

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Use Pyflakes
(require 'pyflakes)

;; Elpy
(elpy-enable)
;; ;; Always use ipython
;; (elpy-use-ipython)
(setq elpy-rpc-backend "rope")

;; Rust
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; Ruby

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Sane indenting
(setq ruby-deep-indent-paren nil)

;; Ruby repl
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; RVM
(global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; Code completion and docs
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Play nice with RVM
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

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

(defun prev-window ()
  (interactive)
  (other-window -1))

;; Shortcuts for going forward and backwards cycling windows
(global-set-key (kbd "C-x p") 'other-window)
(global-set-key (kbd "C-x o") 'prev-window)

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

;; More pronounced cursor
(set-cursor-color "strawberry")

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

;; Use paredit mode
(add-hook 'clojure-mode-hook 'paredit-mode)

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

;; js2 (javascript and jsx)
;; Set to 2 spaces for tabs instead of 4
(setq js2-basic-offset 2)

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

;; Shortcut to show preferred agenda view
(global-set-key (kbd "C-c A") 'org-agenda-and-todos-two-weeks)

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
              (color-org-header "todo:"  "green")
              (color-org-header "tech:"  "blue")
              (color-org-header "personal:" "orange"))))

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
(setq org-agenda-files
      '("~/Dropbox/Private/Clojure/chocolatier/todo.org"
        "~/DropBox/org/"))
(setq org-agenda-text-search-extra-files
      '(agenda-archives
        "~/Dropbox/Private/Clojure/chocolatier/todo.org_archive"))
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c C-a") 'org-agenda)

;; Org mode alerts
(add-to-list 'load-path "~/.emacs.d/vendor/org-alert")
(require 'org-alert)
(setq alert-default-style 'growl)
(setq org-alert-interval "5s")
(setq org-alert-notification-title "Org Mode")
(setq org-alert-enable t)

;; Keyboard shortcut to get to the main todo file
(global-set-key (kbd "C-c T")
                (lambda ()
                  (interactive)
                  (find-file "~/DropBox/org/personal.org")))

;; Archive all DONE tasks
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
      "/DONE" 'tree))

;; Capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/DropBox/org/refile.org")
;; Allow the creation of parent headings when refiling
(setq org-refile-allow-creating-parent-nodes t)
(setq org-capture-templates
      (quote (("t" "To Do" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U" :clock-in t :clock-resume t)
              ("n" "Note" entry (file "~/Dropbox/org/refile.org")
               "* %? %T :note:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
               "* Meeting w/%? %T :meeting:\n%U" :clock-in t :clock-resume t)
              ("i" "Interview - Phone Screen" entry (file "~/Dropbox/org/refile.org")
               "* [PHONE SCREEN] %? %T :interview:\n%U" :clock-in t :clock-resume t)
              ("T" "Interview - Tech" entry (file "~/Dropbox/org/refile.org")
               "* [TECH INTERVIEW] %? %T :interview:\n%U" :clock-in t :clock-resume t))))

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
;; (require 'golden-ratio)
;; (golden-ratio-mode 1)
;; (setq golden-ratio-auto-scale nil)

;; Rope
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  ;; Don't overwrite existing key bindings
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-r")
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil))

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

(defvar centered nil)

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

;; Use ido inside flx buffer too
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
  "save a macro. Take a name as argument
     and save the last defined macro under
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

;; browse-kill-ring with M-y
(browse-kill-ring-default-keybindings)

;; Terminal helper functions
(defun new-term (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term (getenv "SHELL"))
  (rename-buffer buffer-name t))

(defun projectile-term ()
  "Create an ansi-term at the project root"
  (interactive)
  (let ((root (projectile-project-root))
	(buff-name (concat " [term] " (projectile-project-root))))
    (if (get-buffer buff-name)
      (switch-to-buffer-other-window buff-name)
      (progn
	(split-window-sensibly (selected-window))
	(other-window 1)
	(setq default-directory root)
	(ansi-term (getenv "SHELL"))
	(rename-buffer buff-name t)))))

;; Shortcut to create a new term in the current project
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

;; Set default font
;; When using gui emacs with emacsclient, the following makes sure the
;; window is opened with the correct font size otherwise it will
;; revert back to 12px for font size
(add-to-list 'default-frame-alist '(font . "monaco-13"))
;; Make the default face the same font
(set-face-attribute 'default t :font "monaco")
(set-face-attribute 'default nil :height 130)
;; Font for all unicode characters
(set-fontset-font t 'unicode "Font Awesome" nil 'prepend)
