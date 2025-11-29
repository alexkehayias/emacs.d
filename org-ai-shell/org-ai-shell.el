;; Provides a wrapper for the org-ai chat interface using comint.
;; Based off of the tutorial here
;; https://www.masteringemacs.org/article/comint-writing-command-interpreter

(defvar my-org-ai-process nil
  "Variable to store the org-ai process.")

(defvar org-ai-cli-file-path "~/Projects/indexer/bin/chat.sh"
  "Path to the program used by `org-ai-shell'")

(defvar org-ai-cli-arguments '()
  "Commandline arguments to pass to `org-ai'.")

(defvar org-ai-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `org-ai-shell'.")

(defvar org-ai-prompt-regexp "^>>> "
  "Prompt for `org-ai-shell'.")

(defvar org-ai-buffer-name "*Org AI*"
  "Name of the buffer to use for the `org-ai' comint instance.")

(defvar org-ai-markdown-buffer-name "*Org AI Markdown*"
  "Name of the buffer to use for markdown formatted output.")

(defun org-ai-shell ()
  "Run an inferior instance of `org-ai' inside Emacs."
  (interactive)
  (let* ((org-ai-program org-ai-cli-file-path)
         (buffer (get-buffer-create org-ai-buffer-name))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "Org AI" buffer
               org-ai-program nil org-ai-cli-arguments)
        (org-ai-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(defun org-ai--initialize ()
  "Helper function to initialize Org-Ai."
  ;; Set special comint settings
  (setq comint-prompt-read-only t)
  ;; Enable async output processing for markdown formatting
  (add-hook 'comint-output-filter-functions 'org-ai--process-output nil t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'"))

(defun org-ai--process-output (output)
  "Process OUTPUT from comint and format it in markdown."
  (when (and output (not (string-match-p "\\`\\s-*\\'" output)))
    ;; Create a separate markdown buffer for formatted output
    (let ((markdown-buffer (get-buffer-create org-ai-markdown-buffer-name)))
      (with-current-buffer markdown-buffer
        ;; Add timestamp
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d %H:%M:%S") "\n")
        ;; Insert the raw output
        (insert output)
        ;; Enable markdown mode for fontification
        (markdown-mode)
        ;; Show the formatted buffer in a separate window
        (display-buffer markdown-buffer))
      ;; Don't interfere with the original comint buffer's output
      nil)))

(define-derived-mode org-ai-mode comint-mode "Org AI"
  "Major mode for `org-ai-shell'.

\\<org-ai-mode-map>"
  (org-ai--initialize))

(add-hook 'org-ai-mode-hook 'org-ai--initialize)

(provide 'org-ai-shell)
