(defvar sticky-buffers-list nil
  "List of sticky buffers.")

(defface sticky-buffer-face
  '((((background dark)) :background "dark slate gray")
    (((background light)) :background "light goldenrod yellow"))
  "Face for sticky buffers, simulating a different theme effect.")

(defun sticky-buffer-add ()
  "Add current buffer to sticky buffers list."
  (interactive)
  (unless (member (current-buffer) sticky-buffers-list)
    (push (current-buffer) sticky-buffers-list)
    (setq-local buffer-face-mode-face 'sticky-buffer-face)
    (buffer-face-mode 1)
    (message "Buffer %s added to sticky list." (buffer-name))))

(defun sticky-buffer-remove ()
  "Remove current buffer from sticky buffers list."
  (interactive)
  (setq sticky-buffers-list (delq (current-buffer) sticky-buffers-list))
  (buffer-face-mode -1)
  (message "Buffer %s removed from sticky list." (buffer-name)))

(defun sticky-buffer-next-buffer ()
  "Focus the next window displaying a non-sticky buffer."
  (interactive)
  (let ((windows (window-list)))
    (catch 'done
      (dolist (win (append (cdr (member (selected-window) windows)) (list (car windows))))
        (let ((buf (window-buffer win)))
          (unless (member buf sticky-buffers-list)
            (select-window win)
            (throw 'done nil))))
      (message "No non-sticky buffer windows available."))))

(defun sticky-buffer-previous-buffer ()
  "Focus the previous window displaying a non-sticky buffer."
  (interactive)
  (let ((windows (window-list)))
    (catch 'done
      (dolist (win (reverse (cdr (member (selected-window) windows))))
        (let ((buf (window-buffer win)))
          (unless (member buf sticky-buffers-list)
            (select-window win)
            (throw 'done nil))))
      (message "No non-sticky buffer windows available."))))

(defvar sticky-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x p") 'sticky-buffer-next-buffer)
    (define-key map (kbd "C-x o") 'sticky-buffer-previous-buffer)
    map)
  "Keymap for sticky buffer mode.")

(define-minor-mode sticky-buffer-mode
  "Minor mode to make the current buffer sticky."
  :lighter " Sticky"
  :global t
  :keymap sticky-buffer-mode-map)

(provide 'sticky-buffer-mode)
