(defun sacha/org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

(defun sacha/org-link-projects (location)
  "Add link properties between the current subtree and the one specified by LOCATION."
  (interactive
   (list (let ((org-refile-use-cache nil))
           (org-refile-get-location "Location"))))
  (let ((link1 (org-store-link nil)) link2)
    (save-window-excursion
      (org-refile 4 nil location)
      (setq link2 (org-store-link nil))
      (org-set-property "LINK" link1))
    (org-set-property "LINK" link2)))

(defun daniel/insert-link-org-link-with-title (title)
  (interactive "sEnter link title:" title)
  (org-insert-link nil nil title))

(defun sacha/org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n\n" new-date))))

(defun sacha/org-capture-refile-and-jump ()
  (interactive)
  (org-capture-refile)
  (org-refile-goto-last-stored))

(require 'org-clock)
(defun sacha/org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
             (minutes (org-clock-sum-current-item))
             (wpm (/ words minutes)))
        (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
        (kill-new (number-to-string wpm))))))

(defun sacha/org-agenda-for-subtree ()
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (let* ((marker (or (org-get-at-bol 'org-marker)
                         (org-agenda-error)))
             (hdmarker (or (org-get-at-bol 'org-hd-marker) marker))
             (pos (marker-position marker))
             (col (current-column))
             newhead)
        (org-with-remote-undo (marker-buffer marker)
          (with-current-buffer (marker-buffer marker)
            (widen)
            (let ((org-agenda-view-columns-initially t))
              (org-agenda nil "t" 'subtree)))))
    (let ((org-agenda-view-columns-initially t))
      (org-agenda nil "t" 'subtree))))

(defun sacha/org-clock-in-set-state-to-started ()
  "Mark STARTED when clocked in."
  (save-excursion
    (catch 'exit
      (cond
       ((derived-mode-p 'org-agenda-mode)
        (let* ((marker (or (org-get-at-bol 'org-marker)
                           (org-agenda-error)))
               (hdmarker (or (org-get-at-bol 'org-hd-marker) marker))
               (pos (marker-position marker))
               (col (current-column))
               newhead)
          (org-with-remote-undo (marker-buffer marker)
            (with-current-buffer (marker-buffer marker)
              (widen)
              (goto-char pos)
              (org-back-to-heading t)
              (if (org-get-todo-state)
                  (org-todo "STARTED"))))))
       (t (if (org-get-todo-state)
              (org-todo "STARTED")))))))

(defun sacha/org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode) (org-agenda-clock-in) (org-clock-in))
  (call-interactively 'sacha/org-quantified-track))

(defun sacha/org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(defun sacha/org-agenda-project-agenda ()
  "Return the project headline and up to `sacha/org-agenda-limit-items' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))


  (defun sacha/org-agenda-projects-and-tasks (match)
    "Show TODOs for all `org-agenda-files' headlines matching MATCH."
    (interactive "MString: ")
    (let ((todo-only nil))
      (if org-agenda-overriding-arguments
          (setq todo-only (car org-agenda-overriding-arguments)
                match (nth 1 org-agenda-overriding-arguments)))
      (let* ((org-tags-match-list-sublevels
              org-tags-match-list-sublevels)
             (completion-ignore-case t)
             rtn rtnall files file pos matcher
             buffer)
        (when (and (stringp match) (not (string-match "\\S-" match)))
          (setq match nil))
        (setq matcher (org-make-tags-matcher match)
              match (car matcher) matcher (cdr matcher))
        (catch 'exit
          (if org-agenda-sticky
              (setq org-agenda-buffer-name
                    (if (stringp match)
                        (format "*Org Agenda(%s:%s)*"
                                (or org-keys (or (and todo-only "M") "m")) match)
                      (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
          (org-agenda-prepare (concat "TAGS " match))
          (org-compile-prefix-format 'tags)
          (org-set-sorting-strategy 'tags)
          (setq org-agenda-query-string match)
          (setq org-agenda-redo-command
                (list 'org-tags-view `(quote ,todo-only)
                      (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
          (setq files (org-agenda-files nil 'ifmode)
                rtnall nil)
          (while (setq file (pop files))
            (catch 'nextfile
              (org-check-agenda-file file)
              (setq buffer (if (file-exists-p file)
                               (org-get-agenda-file-buffer file)
                             (error "No such file %s" file)))
              (if (not buffer)
                  ;; If file does not exist, error message to agenda
                  (setq rtn (list
                             (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                        rtnall (append rtnall rtn))
                (with-current-buffer buffer
                  (unless (derived-mode-p 'org-mode)
                    (error "Agenda file %s is not in `org-mode'" file))
                  (save-excursion
                    (save-restriction
                      (if org-agenda-restrict
                          (narrow-to-region org-agenda-restrict-begin
                                            org-agenda-restrict-end)
                        (widen))
                      (setq rtn (org-scan-tags 'sacha/org-agenda-project-agenda matcher todo-only))
                      (setq rtnall (append rtnall rtn))))))))
          (if org-agenda-overriding-header
              (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                          nil 'face 'org-agenda-structure) "\n")
            (insert "Headlines with TAGS match: ")
            (add-text-properties (point-min) (1- (point))
                                 (list 'face 'org-agenda-structure
                                       'short-heading
                                       (concat "Match: " match)))
            (setq pos (point))
            (insert match "\n")
            (add-text-properties pos (1- (point)) (list 'face 'org-warning))
            (setq pos (point))
            (unless org-agenda-multi
              (insert "Press `C-u r' to search again with new search string\n"))
            (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
          (org-agenda-mark-header-line (point-min))
          (when rtnall
            (insert (mapconcat 'identity rtnall "\n") ""))
          (goto-char (point-min))
          (or org-agenda-multi (org-agenda-fit-window-to-buffer))
          (add-text-properties (point-min) (point-max)
                               `(org-agenda-type tags
                                                 org-last-args (,todo-only ,match)
                                                 org-redo-cmd ,org-agenda-redo-command
                                                 org-series-cmd ,org-cmd))
          (org-agenda-finalize)
          (setq buffer-read-only t)))))

(defun sacha/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

(defun sacha/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))

(defun sacha/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))

(defun sacha/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(require 'cl)
(defun sacha/org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun sacha/org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun sacha/org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun sacha/org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (sacha/org-compare-dates
      (sacha/org-min-date sched-a deadline-a)
      (sacha/org-min-date sched-b deadline-b)))))

(defun sacha/org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun sacha/org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (sacha/org-complete-cmp a b)
   (sacha/org-date-cmp a b)))

(defun sacha/org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (sacha/org-get-context a))
        (context-b (sacha/org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

(defun sacha/org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (sacha/org-complete-cmp a b)
   (sacha/org-context-cmp a b)
   (sacha/org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))

(defun sacha/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

(defun sacha/quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (sacha/quantified-get-hours x time-summary)) category))))

(defun sacha/org-summarize-focus-areas ()
  "Summarize previous and upcoming tasks as a list."
  (interactive)
  (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
        (line-re sacha/weekly-review-line-regexp)
        (done-re sacha/weekly-done-line-regexp)
        business relationships life business-next relationships-next life-next string
        start end time-summary biz-time)
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (setq time-summary (quantified-summarize-time start end))
    (setq biz-time (sacha/quantified-get-hours "Business" time-summary))
    (save-window-excursion
      (org-agenda nil "W")
      (setq string (buffer-string))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward line-re nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((or (string= (match-string 1) "business") (string= (match-string 1) "tasks"))
            (add-to-list 'business-next (concat "  - [ ] " (match-string 3))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships-next (concat "  - [ ] " (match-string 3))))
           (t (add-to-list 'life-next (concat "  - [ ] " (match-string 3))))))))
    (save-window-excursion
      (org-agenda nil "W")
      (org-agenda-later -1)
      (org-agenda-log-mode 16)
      (setq string (buffer-string))
      ;; Get any completed tasks from the current week as well
      (org-agenda-later 1)
      (org-agenda-log-mode 16)
      (setq string (concat string "\n" (buffer-string)))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward done-re nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((or (string= (match-string 1) "business") (string= (match-string 1) "tasks"))
            (add-to-list 'business (concat "  - [X] " (match-string 2))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships (concat "  - [X] " (match-string 2))))
           (t (add-to-list 'life (concat "  - [X] " (match-string 2))))))))
    (setq string
          (concat
           (format "- *Business* (%.1fh - %d%%)\n" biz-time (/ biz-time 1.68))
           (mapconcat 'identity business "\n") "\n"
           (mapconcat 'identity business-next "\n")
           "\n"
           (format "  - *Earn* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Earn" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Earn" time-summary) (* 0.01 biz-time)))
           (format "  - *Build* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Build" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Build" time-summary) (* 0.01 biz-time)))
           (format "    - *Drawing* (%.1fh)\n"
                   (sacha/quantified-get-hours '("Business - Build - Drawing"
                                                 "Business - Build - Book review")  time-summary))
           (format "    - *Delegation* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Delegation" time-summary))
           (format "    - *Packaging* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Packaging" time-summary))
           (format "    - *Paperwork* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Paperwork"  time-summary))
           (format "  - *Connect* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Connect" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Connect" time-summary) (* 0.01 biz-time)))
           (format "- *Relationships* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours '("Discretionary - Social"
                                                 "Discretionary - Family") time-summary)
                   (/ (sacha/quantified-get-hours '("Discretionary - Social"
                                                    "Discretionary - Family") time-summary) 1.68))
           (mapconcat 'identity (sort relationships 'string<) "\n") "\n"
           (mapconcat 'identity (sort relationships-next 'string<) "\n")
           "\n"
           (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Productive" time-summary) 1.68))
           (format "  - *Emacs* (%.1fh - %d%% of all)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive - Emacs" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Productive - Emacs" time-summary) 1.68))
           (mapconcat 'identity (sort life 'string<) "\n") "\n"
           (mapconcat 'identity (sort life-next 'string<) "\n") "\n"
           (format "  - *Writing* (%.1fh)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive - Writing" time-summary))
           (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Discretionary - Play" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Play" time-summary) 1.68))
           (format "- *Personal routines* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Personal" time-summary)
                   (/ (sacha/quantified-get-hours "Personal" time-summary) 1.68))
           (format "- *Unpaid work* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Unpaid work" time-summary)
                   (/ (sacha/quantified-get-hours "Unpaid work" time-summary) 1.68))
           (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
                   (sacha/quantified-get-hours "Sleep" time-summary)
                   (/ (sacha/quantified-get-hours "Sleep" time-summary) 1.68)
                   (/ (sacha/quantified-get-hours "Sleep" time-summary) 7)
                   )))
    (if (called-interactively-p 'any)
        (insert string)
      string)))

(defun sacha/org-add-line-item-task (task)
  (interactive "MTask: ")
  (org-insert-heading)
  (insert "[ ] " task)
  (let ((org-capture-entry '("t" "Tasks" entry
                             (file+headline "~/personal/organizer.org" "Tasks")
                             "")))
    (org-capture nil "t")
    (insert "TODO " task "\nSCHEDULED: <" (org-read-date) ">")))

(defun sacha/org-prepare-weekly-review ()
  "Prepare weekly review template."
  (interactive)
  (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
        start end)
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (outline-next-heading)
    (insert
     "*** Weekly review: Week ending " (format-time-string "%B %e, %Y" base-date) "  :weekly:\n"
     "*Blog posts*\n\n"
     "*Sketches*\n\n"
     (sacha/flickr-export-and-extract start end) "\n"
     "*Link round-up*\n\n"
     (sacha/evernote-export-and-extract start end)
     "\n\n*Focus areas and time review*\n\n"
     (sacha/org-summarize-focus-areas)
     "\n")))

(defun sacha/org-archive-done-tasks ()
  "Archive finished or cancelled tasks."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

(defun sacha/org-publish-maybe ()
  (interactive)
  (save-excursion
    (when (org-publish-get-project-from-filename
           (buffer-file-name (buffer-base-buffer)) 'up)
      (org-publish-current-file))))

(defun sacha/org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (sacha/org-publish-maybe)
  (browse-url (org-export-output-file-name ".html" nil default-directory)))

(defun sacha/org-html-export-trustingly ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)))

(defun markdown-preview-file-with-marked ()
  "run Marked on the current file (convert it to markdown in advance if the file is *.org)."
  (interactive)
  (if (string= (file-name-extension buffer-file-name) "org")
      (org-md-export-to-markdown) nil)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
           (shell-quote-argument
            (concat (file-name-sans-extension buffer-file-name) ".md")))))
