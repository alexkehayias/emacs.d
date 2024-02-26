#!/bin/sh -e
# This is required by org-roam or it will fail to init
mkdir -p ~/Org/notes
# Load the init-export and install packages
# Note: there is no need to `straight-thaw-versions` since that is
# part of init-export
${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init-export.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook)))'
