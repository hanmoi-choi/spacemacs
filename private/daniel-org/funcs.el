;; These are from http://gitwiki.org/Tests/org-mode.org
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
          "STARTED"))))
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))))

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

(defun markdown-preview-file-with-marked ()
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
