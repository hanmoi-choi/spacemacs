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
        ;; org-mime is installed by `org-plus-contrib'
        (org-mime :location built-in)
        org-pomodoro
        plantuml-mode
        toc-org))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun org/post-init-company ()
    (spacemacs|add-company-hook org-mode)
    (push 'company-capf company-backends-org-mode)))

(defun daniel-org/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :mode ("\\.\\(plantuml\\)$" . puml-mode)
    :init
    (setq plantuml-jar-path "~/.emacs.d/private/bin/plantuml.jar")))

(defun daniel-org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (evil-leader/set-key-for-mode 'org-mode
            "mtp" 'org-plot/gnuplot)))

;; dummy init function to force installation of `org-plus-contrib'
(defun daniel-org/init-org-plus-contrib ())

(defun daniel-org/init-org ()
  (use-package org
    :ensure t
    :mode ("\\.\\(org|org_archive|txt\\)$" . org-mode)
    :init
    (progn
      (custom-set-faces
       '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
       '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
       '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
       '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
       '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
       )

      (setq org-directory "~/Box Sync/org")
      (setq org-mobile-directory "~/Box Sync/MobileOrg")

      ;; GOTO
      (setq org-goto-interface 'outline-path-completion
            org-goto-max-level 10)

      ;; STARTUP
      (setq org-startup-indented t      ;indent heading at start
            org-startup-folded 'content) ;folded content

      (setq org-cycle-include-plain-lists 'integrate
            org-log-done t)

      (setq org-reverse-note-order t
            org-tags-exclude-from-inheritance '("project")
            org-blank-before-new-entry nil)

      (setq org-clock-persist-file
            (concat spacemacs-cache-directory "org-clock-save.el")
            org-src-fontify-natively t
            org-clone-delete-id t
            org-yank-adjusted-subtrees t
            org-agenda-window-setup 'current-window)

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))

      (let ((dir (configuration-layer/get-layer-property 'daniel-org :dir)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))

      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))
      )
    :config
    (progn
      ;;;;;;;;;;;;;;
      ;; Global
      ;;;;;;;;;;;;;;
      ;; Separate drawers for clocking and logs
      (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                                  ("*" . "-")
                                                  ("1." . "-")
                                                  ("1)" . "-")
                                                  ("A)" . "-")
                                                  ("B)" . "-")
                                                  ("a)" . "-")
                                                  ("b)" . "-")
                                                  ("A." . "-")
                                                  ("B." . "-")
                                                  ("a." . "-")
                                                  ("b." . "-"))))
      (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
      (add-to-list 'org-global-properties
                   '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

      (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
      (setq org-time-clocksum-format
            '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
      (setq org-catch-invisible-edits 'error)

      ;; clojure
      (require 'cider)
      (setq org-babel-clojure-backend 'cider)
      (setq org-babel-haskell-backend 'ghc)

      ;; Get this from https://raw.github.com/chenfengyuan/elisp/master/next-spec-day.el
      (load (concat vendor-directory "next-spec-day.el") t)
      ;;;;;;;;;;;;;;
      ;; HTMLIZE
      ;;;;;;;;;;;;;;
      (setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                       ("/" italic "<i>" "</i>")
                                       ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                       ("=" org-code "<code>" "</code>" verbatim)
                                       ("~" org-verbatim "<code>" "</code>" verbatim))))

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

      ;;;;;;;;;;;;;;
      ;; Babel
      ;;;;;;;;;;;;;;
      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (setq org-ditaa-jar-path "~/.emacs.d/private/bin/ditaa.jar"
            org-plantuml-jar-path "~/.emacs.d/private/bin/plantuml.jar"
            org-babel-results-keyword "results")
      (define-key org-src-mode-map (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '")) 'org-edit-src-exit)
      (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
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

      ;;;;;;;;;;;;;;
      ;; Capture
      ;;;;;;;;;;;;;;
      (setq org-capture-templates
            (quote (("t" "TODO" entry
                     (file "~/Box Sync/org/todo/refile.org")
                     "* TODO %^{Task}
CREATED: %<%Y-%m-%d %H:%M>
SCHEDULED: %^t
%? ")
                    ("T" "Quick task" entry
                     (file "~/Box Sync/org/todo/refile.org" "Tasks")
                     "* TODO %^{Task}\nSCHEDULED: %t\n"
                     :immediate-finish t)

                    ("m" "Mail todo" entry (file+headline "~/Box Sync/org/todo/refile.org" "Tasks")
                     "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")

                    ("m" "Meeting" entry
                     (file "~/Box Sync/org/todo/refile.org" "Tasks")
                     "* MEETING %^{Task}\nSCHEDULED: %t\n")

                    ("i" "Interrupting task" entry
                     (file "~/Box Sync/org/todo/refile.org" "Tasks")
                     "* NEXT %^{Task}"
                     :clock-in :clock-resume)

                    ("n" "NOTE" entry (file "~/Box Sync/org/todo/note.org")
                     "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                    )))

      (add-hook 'org-capture-prepare-finalize-hook 'daniel/org-level1-replace)
      (setq org-default-notes-file "~/Box Sync/org/todo/refile.org")

      ;;;;;;;;;;;;;;
      ;; Todo
      ;;;;;;;;;;;;;;
      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))

      (setq org-todo-keyword-faces
            (quote (("TODO" :foreground "orange red" :weight bold)
                    ("NEXT" :foreground "cyan3" :weight bold)
                    ("DONE" :foreground "green yellow" :weight bold)
                    ("WAITING" :foreground "chocolate" :weight bold)
                    ("HOLD" :foreground "magenta" :weight bold)
                    ("CANCELLED" :foreground "dark grey" :weight bold)
                    ("MEETING" :foreground "MediumPurple1" :weight bold))))

      (setq org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                    ("WAITING" ("WAITING" . t))
                    ("HOLD" ("WAITING") ("HOLD" . t))
                    ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

      ;;;;;;;;;;;;;;
      ;; Clock
      ;;;;;;;;;;;;;;
      (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
      ;; Resume clocking task when emacs is restarted
      (org-clock-persistence-insinuate)
      ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
      (setq org-clock-history-length 23)
      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)
      ;; Change tasks to NEXT when clocking in
      (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
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
      (setq org-time-stamp-rounding-minutes (quote (1 1)))
      (setq org-clock-out-remove-zero-time-clocks t)
      (add-hook 'org-clock-in-prepare-hook
                'my/org-mode-ask-effort)
      (setq org-show-notification-handler 'message)
      (setq org-log-into-drawer "LOGBOOK")

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
      (setq org-outline-path-complete-in-steps nil)
      (setq org-fast-tag-selection-single-key (quote expert))
      (setq org-indirect-buffer-display 'current-window)

      ;;;;;;;;;;;;;;
      ;; Refile
      ;;;;;;;;;;;;;;
      (fset 'org-refile-get-location 'my/org-refile-get-location)
      (setq org-refile-use-cache nil)
      (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                       (org-agenda-files :maxlevel . 9))))
      (setq org-refile-use-outline-path t)
      (setq org-refile-allow-creating-parent-nodes t)
      (setq org-refile-target-verify-function 'bh/verify-refile-target)

      ;;;;;;;;;;;;;;
      ;; Agenda
      ;;;;;;;;;;;;;;
      (setq org-agenda-files
            (delq nil
                  (mapcar (lambda (x) (and (file-exists-p x) x))
                          '("~/Box Sync/org/todo/refile.org"
                            "~/Box Sync/org/todo/private.org"
                            "~/Box Sync/org/todo/work.org"))))

      ;; Agenda log mode items to display (closed and state changes by default)
      (defvar my/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")
      (defadvice org-agenda-finalize-entries (around sacha activate)
        (if my/org-agenda-limit-items
            (progn
              (setq list (mapcar 'org-agenda-highlight-todo list))
              (setq ad-return-value
                    (subseq list 0 my/org-agenda-limit-items))
              (when org-agenda-before-sorting-filter-function
                (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
              (setq ad-return-value
                    (mapconcat 'identity
                               (delq nil
                                     (subseq
                                      (sort list 'org-entries-lessp)
                                      0
                                      my/org-agenda-limit-items))
                               "\n")))
          ad-do-it))

      (setq org-agenda-log-mode-items (quote (closed state)))
      (setq org-agenda-clockreport-parameter-plist
            (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
      (setq org-agenda-clock-consistency-checks
            (quote (:max-duration "4:00"
                                  :min-duration 0
                                  :max-gap 0
                                  :gap-ok-around ("4:00"))))
      (setq org-agenda-tags-todo-honor-ignore-options t
            org-agenda-dim-blocked-tasks nil
            org-agenda-span 14
            org-agenda-sticky nil
            org-agenda-inhibit-startup t
            org-agenda-use-tag-inheritance t
            org-agenda-show-log t
            ;; org-agenda-skip-scheduled-if-done t
            ;; org-agenda-skip-deadline-if-done t
            org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
      (setq org-agenda-time-grid
            '((daily today require-timed)
              "----------------"
              (900 1100 1300 1500 1700 1900)))

      (setq org-columns-default-format "%80ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS %10CLOCKSUM")
      (setq org-agenda-sorting-strategy
            '((agenda time-up priority-down tag-up effort-up category-keep)
              (todo user-defined-up todo-state-up priority-down effort-up)
              (tags user-defined-up)
              (search category-keep)))
      (setq org-agenda-cmp-user-defined 'my/org-sort-agenda-items-user-defined)

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
