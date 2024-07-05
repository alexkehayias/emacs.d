;;; org-edit-indirect.el --- Edit Org-mode elements -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; Created: May-2021
;; License: GPL v
;; Package-Requires: ((emacs "25") (edit-indirect "0.1.6"))
;; Version: 1.0.0

;;; Commentary:
;; Edit Org-mode elements like `org-edit-special' but for things that not covered, like quote, verse and comment blocks

(require 'edit-indirect)

(defun org-edit-generic-block (org-element)
  (interactive)
  (let* ((el-type (org-element-type (org-element-context org-element)))
         (parent (org-element-property :parent org-element))
         (beg (pcase el-type
                ((or `quote-block `verse-block `comment-block `plain-list)
                 (org-element-property :contents-begin org-element))
                (_ (org-element-property :begin (or parent org-element)))))
         (end (pcase el-type
                ((or `quote-block `verse-block `comment-block `plain-list)
                 (org-element-property :contents-end org-element))
                (_ (org-element-property :end (or parent org-element))))))
    (edit-indirect-region beg end :display)))

(defun org-edit-special+ (&optional arg)
  "Call a special editor fore the element at point.
This fn extends `org-edit-special', allowing to edit blocks that
the original function doesn't let you."
  (interactive "P")
  (let* ((element (org-element-at-point))
         (context (org-element-context element)))
    (pcase (org-element-type context)
      ((or `quote-block `verse-block `comment-block
           `paragraph `headline `property-drawer
           `plain-list `item)
          (org-edit-generic-block element))
      (_ (org-edit-special arg)))))

(defun org-edit-indirect--before-commit ()
  ;; if not done this way, edit-indirect chews up the EOF and #+end_quote ends
  ;; up appended to the previous line, breaking the structure of the block
  (when (edit-indirect-buffer-indirect-p)
   (end-of-buffer)
   (forward-char -1)
   (when (not (looking-at "$"))
     (end-of-buffer)
     (newline))))

(add-hook 'edit-indirect-before-commit-hook #'org-edit-indirect--before-commit)
(add-hook 'edit-indirect-after-creation-hook #'outline-show-all)

(define-key org-mode-map (kbd "C-c '") #'org-edit-special+)

(provide 'org-edit-indirect)

;;; org-edit-indirect.el ends here