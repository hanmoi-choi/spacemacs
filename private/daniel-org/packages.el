;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; nse: GPLv3

(setq daniel-org-packages
      '(
        company
        gnuplot
        htmlize
        ;; org is installed by `org-plus-contrib'
        (org :location built-in)
        (org-plus-contrib :step pre)
        org-bullets
        ;; org-mime is installed by `org-plus-contrib'
        (org-mime :location built-in)
        org-pomodoro
        (org-jira :location local)
        toc-org))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun org/post-init-company ()
    (spacemacs|add-company-hook org-mode)
    (push 'company-capf company-backends-org-mode)))

(defun daniel-org/init-org-jira ()
  (use-package org-jira
    :load-path "~/.emacs.d/vendor/org-jira/"
    :init
    (progn
      (setq jiralib-url "https://jira.ibsglobalweb.com:80"))))

(defun daniel-org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (evil-leader/set-key-for-mode 'org-mode
            "mtp" 'org-plot/gnuplot)))

;; dummy init function to force installation of `org-plus-contrib'
(defun daniel-org/init-org-plus-contrib ())

(defun daniel-org/init-org ()
  (use-package org
    :mode ("\\.\\(org|org_archive|txt\\)$" . org-mode)
    :defer t
    :init
    (progn
      (setq org-clock-persist-file
            (concat spacemacs-cache-directory "org-clock-save.el")
            org-log-done t
            org-startup-with-inline-images t
            org-src-fontify-natively t
            org-startup-indented t
            org-cycle-include-plain-lists t
            org-clone-delete-id t
            org-agenda-window-setup 'current-window)

      (setq mail-setup-hook
            (quote (orgstruct++-mode
                    (lambda nil (setq fill-column 80) (flyspell-mode 1))
                    turn-on-auto-fill
                    bbdb-define-all-aliases)))

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))

      (let ((dir (configuration-layer/get-layer-property 'daniel-org :dir)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))

      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))

      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-org (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -8))))
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

      (eval-after-load "org-agenda"
        '(progn
           (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
           (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
           ;; Since we override SPC, let's make RET do that functionality
           (define-key org-agenda-mode-map
             (kbd "RET") 'org-agenda-show-and-scroll-up)
           (define-key org-agenda-mode-map
             (kbd "SPC") evil-leader--default-map))))
    :config
    (progn
      (add-hook 'org-after-todo-state-change-hook 'bh/mark-parent-tasks-started 'append)
      (spacemacs/declare-prefix "O" "org-mode")
      (evil-leader/set-key
        "Oa" 'org-agenda
        "Ol" 'org-store-link
        "Oc" 'org-cycle-agenda-files
        "OI" 'bh/clock-in
        "OO" 'bh/clock-out
        "OL" 'bh/clock-in-last-task
        "Ot" 'bh/insert-inactive-timestamp
        "Ou" 'bh/untabify
        "Os" 'org-iswitchb)

      (global-set-key (kbd "C-c c") 'org-capture)
      (global-set-key (kbd "C-c l") 'org-store-link)
      (global-set-key (kbd "<f12>") 'org-agenda)
      (global-set-key (kbd "<f11>") 'org-clock-goto)

      (add-hook 'org-mode-hook
                (lambda ()
                  (if window-system            nil
                    (progn
                      (define-key org-mode-map (kbd "C-M-j") 'org-meta-return)))))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
                'append)

      (define-key org-mode-map (kbd "C-x C-v" ) 'markdown-preview-file-with-marked)

      (setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                       ("/" italic "<i>" "</i>")
                                       ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                       ("=" org-code "<code>" "</code>" verbatim)
                                       ("~" org-verbatim "<code>" "</code>" verbatim))))

      (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
      (setq org-time-clocksum-format
            '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
      (setq org-catch-invisible-edits 'error)

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

      (require 'org-indent)
      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '")) 'org-edit-src-exit)
      (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

      (setq org-goto-interface 'outline
            org-goto-max-level 10
            org-startup-folded 'content
            org-cycle-include-plain-lists 'integrate
            org-log-done t
            org-ditaa-jar-path "~/Dropbox/Apps/spacemacs/private/bin/ditaa0_9.jar"
            org-plantuml-jar-path "~/Dropbox/Apps/spacemacs/private/bin/plantuml.jar"
            org-babel-results-keyword "results")

      (org-babel-do-load-languages
       (quote org-babel-load-languages)
       (quote ((emacs-lisp . t)
               (dot . t)
               (ditaa . t)
               (python . t)
               (ruby . t)
               (gnuplot . t)
               (clojure . t)
               (sh . t)
               (org . t)
               (plantuml . t)
               (latex . t))))

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
      (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
      (add-hook 'org-capture-prepare-finalize-hook 'daniel/org-level1-replace)

      (setq org-reverse-note-order t
            org-refile-use-outline-path nil
            org-refile-allow-creating-parent-nodes t
            org-refile-use-cache nil
            org-refile-targets '((org-agenda-files . (:maxlevel . 5)))
            org-tags-exclude-from-inheritance '("project")
            org-blank-before-new-entry nil)

      (setq org-default-notes-file "~/Dropbox/org/todo/refile.org")

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

      (setq org-todo-keyword-faces
            (quote (("TODO" :foreground "red" :weight bold)
                    ("NEXT" :foreground "blue" :weight bold)
                    ("DONE" :foreground "forest green" :weight bold)
                    ("WAITING" :foreground "orange" :weight bold)
                    ("HOLD" :foreground "magenta" :weight bold)
                    ("CANCELLED" :foreground "forest green" :weight bold)
                    ("MEETING" :foreground "forest green" :weight bold)
                    ("PHONE" :foreground "forest green" :weight bold))))

      (setq org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                    ("WAITING" ("WAITING" . t))
                    ("HOLD" ("WAITING") ("HOLD" . t))
                    (done ("WAITING") ("HOLD"))
                    ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

      ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol

      (defun my/org-contacts-template-email (&optional return-value)
        "Try to return the contact email for a template.
If not found return RETURN-VALUE or something that would ask the user."
        (or (cadr (if (gnus-alive-p)
                      (gnus-with-article-headers
                       (mail-extract-address-components
                        (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
            return-value
            (concat "%^{" org-contacts-email-property "}p")))

      (defvar my/org-basic-task-template "* TODO %^{Task}
SCHEDULED: %^t
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
%<%Y-%m-%d %H:%M>
%?
" "Basic task data")

      (setq org-capture-templates
            (quote (("t" "TODO" entry
                     (file "~/Dropbox/org/todo/refile.org")
                     ,my/org-basic-task-template)
                    ("i" "Interrupting task" entry
                     (file+headline "~/personal/organizer.org" "Tasks")
                     "* STARTED %^{Task}"
                     :clock-in :clock-resume)
                    ("n" "NOTE" entry (file "~/Dropbox/org/todo/refile.org")
                     "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("w" "WORK LOG" plain
                     (file+datetree+prompt "~/Dropbox/org/todo/worklog.org")
                     "%K - %a\n%i\n%?\n"
                     :unnarrowed t)
                    ("h" "HABIT" entry (file "~/Dropbox/org/todo/refile.org")
                     "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

      (setq org-agenda-files
            (delq nil
                  (mapcar (lambda (x) (and (file-exists-p x) x))
                          '("~/Dropbox/org/todo/refile.org"
                            "~/Dropbox/org/todo/private.org"
                            "~/Dropbox/org/todo/code.org"
                            "~/Dropbox/org/todo/work.org"))))

      ;; Resume clocking task when emacs is restarted
      (org-clock-persistence-insinuate)
      ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
      (setq org-clock-history-length 23)
      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)
      ;; Change tasks to NEXT when clocking in
      (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
      ;; Separate drawers for clocking and logs
      (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)
      ;; Clock out when moving task to a done state
      (setq org-clock-out-when-done t)
      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (setq org-clock-persist t)
      ;; Do not prompt to resume an active clock
      (setq org-clock-persist-query-resume nil)
      ;; Enable auto clock resolution for finding open clocks
      (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
      ;; Include current clocking task in clock reports
      (setq org-clock-report-include-clocking-task t)
      (setq bh/keep-clock-running nil)
      (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

      (require 'org-id)
      (defun bh/clock-in-task-by-id (id)
        "Clock in a task by id"
        (org-with-point-at (org-id-find id 'marker)
          (org-clock-in nil)))

      (defun bh/clock-in-last-task (arg)
        "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
        (interactive "p")
        (let ((clock-in-to-task
               (cond
                ((eq arg 4) org-clock-default-task)
                ((and (org-clock-is-active)
                      (equal org-clock-default-task (cadr org-clock-history)))
                 (caddr org-clock-history))
                ((org-clock-is-active) (cadr org-clock-history))
                ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
                (t (car org-clock-history)))))
          (widen)
          (org-with-point-at clock-in-to-task
            (org-clock-in nil))))
      (setq org-time-stamp-rounding-minutes (quote (1 1)))
      (setq org-agenda-clock-consistency-checks
            (quote (:max-duration "4:00"
                                  :min-duration 0
                                  :max-gap 0
                                  :gap-ok-around ("4:00"))))
      (setq org-clock-out-remove-zero-time-clocks t)
      (setq org-agenda-clockreport-parameter-plist
            (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
                                        ; Set default column view headings: Task Effort Clock_Summary
      (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
      (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                          ("STYLE_ALL" . "habit"))))
      ;; Agenda log mode items to display (closed and state changes by default)
      (setq org-agenda-log-mode-items (quote (closed state)))
                                        ; Tags with fast selection keys
      (setq org-tag-alist (quote ((:startgroup)
                                  ("@errand" . ?e)
                                  ("@work" . ?o)
                                  ("@home" . ?H)
                                  (:endgroup)
                                  ("WAITING" . ?w)
                                  ("HOLD" . ?h)
                                  ("PERSONAL" . ?P)
                                  ("WORK" . ?W)
                                  ("NOTE" . ?n)
                                  ("CANCELLED" . ?c)
                                  ("FLAGGED" . ??))))

                                        ; Allow setting single tags without the menu
      (setq org-fast-tag-selection-single-key (quote expert))
                                        ; For tag searches ignore tasks with scheduled and deadline dates
      (setq org-agenda-tags-todo-honor-ignore-options t)

      (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                       (org-agenda-files :maxlevel . 9))))
      (setq org-refile-use-outline-path t)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-allow-creating-parent-nodes (quote confirm))
      (setq org-indirect-buffer-display 'current-window)
      (defun bh/verify-refile-target ()
        "Exclude todo keywords with a done state from refile targets"
        (not (member (nth 2 (org-heading-components)) org-done-keywords)))

      (setq org-refile-target-verify-function 'bh/verify-refile-target)

      (setq org-agenda-dim-blocked-tasks nil
            org-agenda-compact-blocks t
            org-agenda-sticky nil
            org-agenda-inhibit-startup t
            org-agenda-show-log t)

      (setq org-agenda-custom-commands
            (quote (("w" "Tasks waiting on something" tags "WAITING/!"
                     ((org-use-tag-inheritance nil)
                      (org-agenda-todo-ignore-scheduled nil)
                      (org-agenda-todo-ignore-deadlines nil)
                      (org-agenda-todo-ignore-with-date nil)
                      (org-agenda-overriding-header "Waiting Tasks")))
                    ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
                     ((org-agenda-todo-ignore-with-date nil)
                      (org-agenda-todo-ignore-deadlines nil)
                      (org-agenda-todo-ignore-scheduled nil)
                      (org-agenda-overriding-header "Tasks to Refile")))
                    ("N" "Notes" tags "NOTE"
                     ((org-agenda-overriding-header "Notes")))
                    ("s" "STARTED" tags-todo "-WAITING-CANCELLED/!STARTED"
                     ((org-agenda-overriding-header "STARTED Tasks")))
                    ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
                     ((org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-overriding-header "Projects")))
                    ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
                     ((org-agenda-skip-function 'bh/skip-projects)
                      (org-agenda-overriding-header "Other Non-Project Tasks")))
                    ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELLED"
                     ((org-agenda-overriding-header "Tasks to Archive")))
                    ("h" "Habits" tags "STYLE=\"habit\""
                     ((org-agenda-todo-ignore-with-date nil)
                      (org-agenda-todo-ignore-scheduled nil)
                      (org-agenda-todo-ignore-deadlines nil)
                      (org-agenda-overriding-header "Habits")))
                    ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
                     ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-overriding-header "Stuck Projects")))
                    ("c" "Select default clocking task" tags "LEVEL=2-REFILE"
                     ((org-agenda-skip-function
                       '(org-agenda-skip-subtree-if 'notregexp "^\\*\\* Organization"))
                      (org-agenda-overriding-header "Set default clocking task with C-u C-u I")))))))))

(defun daniel-org/init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init
    (progn
      (add-hook 'org-mode-hook 'org-bullets-mode)
      (setq org-bullets-bullet-list
            '(;;; Large
              "⦿"
              "◉"
              "○"
              "◆"
              "◆"
              "◆"
              "◆")))))

(defun daniel-org/init-org-mime ()
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
    :init
    (progn
      (evil-leader/set-key-for-mode 'message-mode
        "mM" 'org-mime-htmlize)
      (evil-leader/set-key-for-mode 'org-mode
        "mm" 'org-mime-org-buffer-htmlize))))

(defun daniel-org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (evil-leader/set-key-for-mode 'org-mode
        "mp" 'org-pomodoro))))

(defun daniel-org/init-toc-org ()
  (use-package toc-org
    :defer t
    :init
    (progn
      (setq toc-org-max-depth 10)
      (add-hook 'org-mode-hook 'toc-org-enable))))

(defun daniel-org/init-htmlize ()
  (use-package htmlize
    :defer t))
