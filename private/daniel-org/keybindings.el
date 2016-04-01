(eval-after-load 'org
  '(progn
     ;;Leader
     (evil-leader/set-key-for-mode 'org-mode
       "m'" 'org-edit-special
       "md" 'org-deadline
       "me" 'org-export-dispatch
       "mf" 'org-set-effort
       "m:" 'org-set-tags

       "mb" 'org-tree-to-indirect-buffer
       "mA" 'org-archive-subtree
       "ml" 'org-open-at-point
       "mT" 'org-show-todo-tree

       "m." 'org-time-stamp

       ;; headings
       "mhi" 'org-insert-heading-after-current
       "mhI" 'org-insert-heading

       ;; More cycling options (timestamps, headlines, items, properties)
       "mL" 'org-shiftright
       "mH" 'org-shiftleft
       "mJ" 'org-shiftdown
       "mK" 'org-shiftup

       ;; Subtree editing
       "mSl" 'org-demote-subtree
       "mSh" 'org-promote-subtree
       "mSj" 'org-move-subtree-down
       "mSk" 'org-move-subtree-up

       ;; tables
       "mta" 'org-table-align
       "mtb" 'org-table-blank-field
       "mtc" 'org-table-convert
       "mtdc" 'org-table-delete-column
       "mtdr" 'org-table-kill-row
       "mte" 'org-table-eval-formula
       "mtE" 'org-table-export
       "mth" 'org-table-previous-field
       "mtH" 'org-table-move-column-left
       "mtic" 'org-table-insert-column
       "mtih" 'org-table-insert-hline
       "mtiH" 'org-table-hline-and-move
       "mtir" 'org-table-insert-row
       "mtI" 'org-table-import
       "mtj" 'org-table-next-row
       "mtJ" 'org-table-move-row-down
       "mtK" 'org-table-move-row-up
       "mtl" 'org-table-next-field
       "mtL" 'org-table-move-column-right
       "mtn" 'org-table-create
       "mtN" 'org-table-create-with-table.el
       "mtr" 'org-table-recalculate
       "mts" 'org-table-sort-lines
       "mttf" 'org-table-toggle-formula-debugger
       "mtto" 'org-table-toggle-coordinate-overlays
       "mtw" 'org-table-wrap-region

       "mI" 'org-clock-in

       (if dotspacemacs-major-mode-leader-key
           (concat "m" dotspacemacs-major-mode-leader-key)
         "m,") 'org-ctrl-c-ctrl-c
         "mn" 'org-narrow-to-subtree
         "mN" 'widen
         "mO" 'org-clock-out
         "mQ" 'org-clock-cancel
         "mR" 'org-refile
         "ms" 'org-schedule

         ;; insertion of common elements
         "mil" 'org-insert-link
         "mif" 'org-footnote-new
         "mik" 'spacemacs/insert-keybinding-org

         ;; images and other link types have no commands in org mode-line
         ;; could be inserted using yasnippet?
         ;; region manipulation
         "mxb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
         "mxc" (spacemacs|org-emphasize spacemacs/org-code ?~)
         "mxi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
         "mxr" (spacemacs|org-emphasize spacemacs/org-clear ? )
         "mxs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
         "mxu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
         "mxv" (spacemacs|org-emphasize spacemacs/org-verbose ?=))
     (spacemacs/declare-prefix "O" "org-mode")
     (evil-leader/set-key
       "Ot" 'org-toggle-inline-images
       "OT" 'org-toggle-link-display
       "Oc" 'org-capture
       "Of" 'org-mobile-pull
       "Op" 'org-mobile-push
       "Oa" 'org-agenda
       "Ol" 'org-store-link
       "Oc" 'org-cycle-agenda-files
       "OO" 'bh/clock-out
       "OL" 'bh/clock-in-last-task
       "Ot" 'bh/insert-inactive-timestamp
       "Os" 'org-iswitchb)

     ;; Global
     (global-set-key (kbd "C-c c") 'org-capture)
     (global-set-key (kbd "C-c l") 'org-store-link)
     (global-set-key (kbd "C-c j") 'org-clock-goto)
     (global-set-key (kbd "C-c <f10>") 'org-clock-goto)
     (global-set-key (kbd "C-c <f11>") 'org-agenda-list)
     (global-set-key (kbd "C-c <f12>") 'org-agenda)
     ;; Mode
     (add-hook 'org-mode-hook
               (lambda ()
                 (if window-system            nil
                   (progn
                     (define-key org-mode-map (kbd "C-M-j") 'org-meta-return)))))

     (bind-key "C-c k" 'org-cut-subtree org-mode-map)

    (define-key org-mode-map [(control meta shift right)] nil)
    (define-key org-mode-map [(control meta shift left)] nil)

     (eval-after-load 'org-capture
       '(progn
          (define-key org-capture-mode-map (kbd "C-c C-r") 'my/org-capture-refile-and-jump)
          ))))

(eval-after-load 'org-agenda
  '(progn
     (define-key org-agenda-mode-map "x" 'my/org-agenda-done)
     (define-key org-agenda-mode-map "X" 'my/org-agenda-mark-done-and-add-followup)
     (define-key org-agenda-mode-map "N" 'my/org-agenda-new)
     (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
     (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
     (bind-key "Y" 'org-agenda-todo-yesterday org-agenda-mode-map)
     ;; Since we override SPC, let's make RET do that functionality
     (define-key org-agenda-mode-map
       (kbd "RET") 'org-agenda-show-and-scroll-up)
     ))
