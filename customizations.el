;; Custom set variables go here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(background-color "#7f7f7f")
 '(background-mode dark)
 '(cursor-color "#5c5cff")
 '(custom-enabled-themes (quote (molokai)))
 '(custom-safe-themes t)
 '(elpy-default-minor-modes
   (quote
    (eldoc-mode flymake-mode yas-minor-mode auto-complete-mode)))
 '(fci-rule-color "#d9d9d9")
 '(flycheck-javascript-flow-args nil)
 '(foreground-color "#5c5cff")
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (helm-ag rotate ox-jira powerline yaml-mode web-mode virtualenvwrapper use-package twilight-bright-theme sws-mode sos smex smartparens slack sass-mode robe rainbow-delimiters racer projectile-ripgrep prodigy popwin paredit pallet ox-reveal ox-gfm org-alert nyan-mode multiple-cursors markdown-mode magit keyfreq json-mode jade-mode iedit idle-highlight-mode htmlize helm-projectile handlebars-mode golden-ratio git-gutter flymake-ruby flycheck-rust flycheck-flow flycheck-cask flx-ido expand-region exec-path-from-shell elpy dumb-jump drag-stuff doom-themes company-flow color-theme coffee-mode cargo browse-kill-ring ace-jump-mode)))
 '(send-mail-function nil)
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#bc8383")
     (40 . "#cc9393")
     (60 . "#dfaf8f")
     (80 . "#d0bf8f")
     (100 . "#e0cf9f")
     (120 . "#f0dfaf")
     (140 . "#5f7f5f")
     (160 . "#7f9f7f")
     (180 . "#8fb28f")
     (200 . "#9fc59f")
     (220 . "#afd8af")
     (240 . "#bfebbf")
     (260 . "#93e0e3")
     (280 . "#6ca0a3")
     (300 . "#7cb8bb")
     (320 . "#8cd0d3")
     (340 . "#94bff3")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1B1D1E" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Monaco"))))
 '(custom-variable-tag ((t (:foreground "color-27" :weight bold))))
 '(erc-input-face ((t (:foreground "color-33"))))
 '(erc-my-nick-face ((t (:foreground "blue" :weight bold))))
 '(flymake-errline ((t (:background "color-52"))))
 '(font-lock-variable-name-face ((t (:foreground "#fd971f"))))
 '(fringe ((t (:foreground "#455354" :background "#232526"))))
 '(highlight-indent-face ((t (:background "#808080"))))
 '(hl-line ((((class color) (min-colors 89)) (:background "#232526")) (t :weight bold)))
 '(linum ((t (:foreground "#bcbcbc" :background "#232526"))))
 '(match ((t (:background "color-28"))))
 '(minibuffer-prompt ((t (:foreground "color-33"))))
 '(mode-line ((t (:background "#232526" :foreground "#F8F8F2" :box (:line-width 8 :color "default")))))
 '(mode-line-inactive ((t (:foreground "#232526" :background "#232526" :box nil))))
 '(org-clock-overlay ((t (:background "gray10" :foreground "white"))))
 '(org-column ((t (:background "black" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-document-info ((t (:foreground "dark gray"))))
 '(org-document-title ((t (:foreground "dark gray" :weight bold))))
 '(org-hide ((t (:foreground "color-16"))))
 '(org-level-4 ((t (:weight semi-light))))
 '(org-todo ((t (:foreground "Pink" :weight bold))))
 '(org-warning ((t (:background "dark red" :foreground "White"))))
 '(term-underline ((t (:foreground "dark cyan" :underline t))))
 '(widget-field ((t nil))))
