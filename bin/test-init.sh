#!/bin/sh -e
# This is required by org-roam or it will fail to init
mkdir -p ~/Org/notes
# Load the init file and install packages
${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook))
                           (straight-thaw-versions))'
