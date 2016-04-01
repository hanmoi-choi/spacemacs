;; These are from http://doc.norang.ca/org-mode.html
(setq bh/keep-clock-running nil)
(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

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

(defmacro spacemacs|org-emphasize (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
          (org-emphasize ,char)))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-shifttab)
    (org-reveal)
    (org-cycle)))

(defun bh/untabify ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (let ((has-subtask) (subtree-end (save-excursion (org-end-of-subtree t))))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      has-subtask))

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (has-next (save-excursion
                       (forward-line 1)
                       (and (< (point) subtree-end)
                            (re-search-forward "^\\*+ NEXT " subtree-end t)))))
      (if (and (bh/is-project-p) (not has-next))
          nil ; a stuck project, has subtasks but no next task
        subtree-end)))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (bh/is-project-p)
          nil
        subtree-end)))

  (defun bh/skip-projects ()
    "Skip trees that are projects"
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (bh/is-project-p)
          subtree-end
        nil)))

(defun bh/clock-in-to-next (kw)
  "Switch task from TODO to STARTED when clocking in.
  Skips capture tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "NEXT"))))

(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(defun bh/weekday-p ()
  (let ((wday (nth 6 (decode-time))))
    (and (< wday 6) (> wday 0))))

(defun bh/working-p ()
  (let ((hour (nth 2 (decode-time))))
    (and (bh/weekday-p) (or (and (>= hour 8) (<= hour 11))
                            (and (>= hour 13) (<= hour 16))))))

(defun bh/clock-in ()
  (interactive)
  (setq bh/keep-clock-running t)
  (if (marker-buffer org-clock-default-task)
      (unless (org-clock-is-active)
        (bh/clock-in-default-task))
    (unless (marker-buffer org-clock-default-task)
      (org-agenda nil "c"))))

(defun bh/clock-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running (not org-clock-clocking-in) (marker-buffer org-clock-default-task))
    (bh/clock-in-default-task)))

(defun bh/org-auto-exclude-function (tag)
  (and (cond
        ((string= tag "@home")
         (bh/working-p))
        ((string= tag "@work")
         (not (bh/working-p)))
        ((or (string= tag "@errand") (string= tag "@phone"))
         (let ((hour (nth 2 (decode-time))))
           (or (< hour 8) (> hour 21)))))
       (concat "-" tag)))

;; Mark parent tasks as started
(defvar bh/mark-parent-tasks-started nil)

(defun bh/mark-parent-tasks-started ()
  "Visit each parent task and change TODO states to STARTED"
  (unless bh/mark-parent-tasks-started
    (when (equal org-state "STARTED")
      (let ((bh/mark-parent-tasks-started t))
        (save-excursion
          (while (org-up-heading-safe)
            (when (member (nth 2 (org-heading-components)) (list "TODO" "NEXT"))
              (org-todo "STARTED"))))))))

(defun preview-file-with-marked2 ()
  "run Marked on the current file (convert it to markdown in advance if the file is *.org)."
  (interactive)
  (if (string= (file-name-extension buffer-file-name) "org")
      (org-md-export-to-markdown) nil)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
           (shell-quote-argument
            (concat (file-name-sans-extension buffer-file-name) ".md")))))

(defun daniel/org-level1-replace ()
  (interactive)
  (goto-char 1)
  (progn
    (replace-string "INBOX" "Archive")))

  (defvar gjg/capture-phone-record nil
    "Either BBDB record vector, or person's name as a string, or nil")

  (defun bh/phone-call ()
    (interactive)
    (let* ((myname (completing-read "Who is calling? " (bbdb-hashtable) 'bbdb-completion-predicate 'confirm))
           (my-bbdb-name (if (> (length myname) 0) myname nil)))
      (setq gjg/capture-phone-record
            (if my-bbdb-name
                (first (or (bbdb-search (bbdb-records) my-bbdb-name nil nil)
                           (bbdb-search (bbdb-records) nil my-bbdb-name nil)))
              myname))
      (other-window 1)
      (let ((org-capture-templates '(("P" "Phone" entry (file "~/Dropbox/org/todo/refile.org") "* TODO Phone %(gjg/bbdb-name) - %(gjg/bbdb-company)               :PHONE:\n  %U\n  %?" :clock-in t :clock-resume t))))
        (org-capture))))

  (defun gjg/bbdb-name ()
    "Return full name of saved bbdb record, or empty string - for use in Capture templates"
    (if (and gjg/capture-phone-record (vectorp gjg/capture-phone-record))
        (concat "[[bbdb:"
                (bbdb-record-name gjg/capture-phone-record) "]["
                (bbdb-record-name gjg/capture-phone-record) "]]")
      "NAME"))

  (defun gjg/bbdb-company ()
    "Return company of saved bbdb record, or empty string - for use in Capture templates"
    (if (and gjg/capture-phone-record (vectorp gjg/capture-phone-record))
        (or (bbdb-record-company gjg/capture-phone-record) "")
      "COMPANY"))


(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))
(defvar bh/project-list nil)
(defvar bh/hide-scheduled-and-waiting-next-tasks nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

;; These function is from http://pages.sachachua.com/.emacs.d/Sacha.html#org-files
(defun my/org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n\n" new-date))))

(defun my/org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (if (gnus-alive-p)
                (gnus-with-article-headers
                 (mail-extract-address-components
                  (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))

  (defun my/helm-org-refile-read-location (tbl)
    (setq my/helm-org-refile-locations tbl)
    (helm
     (list
      ;; (helm-build-sync-source "Today's tasks"
      ;;   :candidates (mapcar (lambda (a) (cons (car a) a))
      ;;                       (my/org-get-todays-items-as-refile-candidates))
      ;;   :action '(("Select" . identity)
      ;;             ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
      ;;             ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
      ;;   :history 'org-refile-history)
      (helm-build-sync-source "Refile targets"
        :candidates (mapcar (lambda (a) (cons (car a) a)) tbl)
        :action '(("Select" . identity)
                  ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
                  ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
        :history 'org-refile-history)
      (helm-build-dummy-source "Create task"
        :action (helm-make-actions
                 "Create task"
                 'my/helm-org-create-task)))))

(defun my/org-refile-get-location (&optional prompt default-buffer new-nodes no-exclude)
    "Prompt the user for a refile location, using PROMPT.
  PROMPT should not be suffixed with a colon and a space, because
  this function appends the default value from
  `org-refile-history' automatically, if that is not empty.
  When NO-EXCLUDE is set, do not exclude headlines in the current subtree,
  this is used for the GOTO interface."
    (let ((org-refile-targets org-refile-targets)
          (org-refile-use-outline-path org-refile-use-outline-path)
          excluded-entries)
      (when (and (derived-mode-p 'org-mode)
                 (not org-refile-use-cache)
                 (not no-exclude))
        (org-map-tree
         (lambda()
           (setq excluded-entries
                 (append excluded-entries (list (org-get-heading t t)))))))
      (setq org-refile-target-table
            (org-refile-get-targets default-buffer excluded-entries)))
    (unless org-refile-target-table
      (user-error "No refile targets"))
    (let* ((cbuf (current-buffer))
           (partial-completion-mode nil)
           (cfn (buffer-file-name (buffer-base-buffer cbuf)))
           (cfunc (if (and org-refile-use-outline-path
                           org-outline-path-complete-in-steps)
                      'org-olpath-completing-read
                    'org-icompleting-read))
           (extra (if org-refile-use-outline-path "/" ""))
           (cbnex (concat (buffer-name) extra))
           (filename (and cfn (expand-file-name cfn)))
           (tbl (mapcar
                 (lambda (x)
                   (if (and (not (member org-refile-use-outline-path
                                         '(file full-file-path)))
                            (not (equal filename (nth 1 x))))
                       (cons (concat (car x) extra " ("
                                     (file-name-nondirectory (nth 1 x)) ")")
                             (cdr x))
                     (cons (concat (car x) extra) (cdr x))))
                 org-refile-target-table))
           (completion-ignore-case t)
           cdef
           (prompt (concat prompt
                           (or (and (car org-refile-history)
                                    (concat " (default " (car org-refile-history) ")"))
                               (and (assoc cbnex tbl) (setq cdef cbnex)
                                    (concat " (default " cbnex ")"))) ": "))
           pa answ parent-target child parent old-hist)
      (setq old-hist org-refile-history)
      ;; Use Helm's sources instead
      (setq answ (my/helm-org-refile-read-location tbl))
      (cond
       ((and (stringp answ)
             (setq pa (org-refile--get-location answ tbl)))
        (org-refile-check-position pa)
        (when (or (not org-refile-history)
                  (not (eq old-hist org-refile-history))
                  (not (equal (car pa) (car org-refile-history))))
          (setq org-refile-history
                (cons (car pa) (if (assoc (car org-refile-history) tbl)
                                   org-refile-history
                                 (cdr org-refile-history))))
          (if (equal (car org-refile-history) (nth 1 org-refile-history))
              (pop org-refile-history)))
        (setq my/org-refile-last-location pa)
        pa)
       ((and (stringp answ) (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ))
        (setq parent (match-string 1 answ)
              child (match-string 2 answ))
        (setq parent-target (org-refile--get-location parent tbl))
        (when (and parent-target
                   (or (eq new-nodes t)
                       (and (eq new-nodes 'confirm)
                            (y-or-n-p (format "Create new node \"%s\"? "
                                              child)))))
          (org-refile-new-child parent-target child)))
       ((listp answ) answ) ;; Sacha: Helm returned a refile location
       ((not (equal answ t))
        (user-error "Invalid target location")))))

(defun my/org-refile-get-location-by-substring (regexp &optional file)
  "Return the refile location identified by REGEXP."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find regexp org-refile-target-table
           :test
           (lambda (a b)
             (and
              (string-match a (car b))
              (or (null file)
                  (string-match file (elt b 1)))))))
(defun my/org-refile-subtree-to (name)
  (org-refile nil nil (my/org-refile-get-location-exact name)))

 (defun my/org-refile-get-location-exact (name &optional file)
  "Return the refile location identified by NAME."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find name org-refile-target-table
           :test (lambda (a b)
                 (and (string-equal a (car b))
              (or (null file)
                  (string-match file (elt b 1)))))))
;; Example: (my/org-clock-in-refile "Off my computer")
 (defun my/org-clock-in-refile (location &optional file)
  "Clocks into LOCATION.
LOCATION and FILE can also be regular expressions for `my/org-refile-get-location-by-substring'."
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my/org-refile-get-location-by-substring location file)))
      (org-refile 4 nil location)
      (org-clock-in))))

 (defun my/org-finish-previous-task-and-clock-in-new-one (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (org-clock-goto)
    (org-todo 'done))
  (my/org-clock-in-and-track-by-name location file))

(defun my/org-clock-in-and-track-by-name (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my/org-refile-get-location-exact location file)))
      (org-refile 4 nil location)
      (my/org-clock-in-and-track))))
(defun my/org-off-my-computer (category)
  (interactive "MCategory: ")
  (my/org-clock-in-refile "Off my computer")
  (quantified-track category))

(defun my/org-capture-refile-and-jump ()
  (interactive)
  (org-capture-refile)
  (org-refile-goto-last-stored))

(defun my/org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))

(defun my/org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(defun my/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit

(defun my/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition

(require 'cl)
(defun my/org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun my/org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun my/org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun my/org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (my/org-compare-dates
      (my/org-min-date sched-a deadline-a)
      (my/org-min-date sched-b deadline-b)))))

(defun my/org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun my/org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (my/org-complete-cmp a b)
   (my/org-date-cmp a b)))

(defun my/org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (my/org-get-context a))
        (context-b (my/org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

(defun my/org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (my/org-complete-cmp a b)
   (my/org-context-cmp a b)
   (my/org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))

;; These are from http://gitwiki.org/Tests/org-mode.org
(defun my/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
