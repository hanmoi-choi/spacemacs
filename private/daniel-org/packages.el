;;; packages.el --- daniel-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq daniel-org-packages
      '(
        org
        htmlize
        org-bullets
        org-pomodoro
        toc-org
        org-reveal
        kanban))

;; List of packages to exclude.
(setq daniel-org-excluded-packages '())

(defun daniel-org/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :config
    (progn
      (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")))
  )


(defun daniel-org/init-org-jira ()
  (use-package org-jira
    ))

(defun daniel-org/init-kanban ()
  (use-package kanban
    ))

(defun daniel-org/init-plantuml-mode()
  (use-package plantuml-mode
    ))

(defun daniel-org/init-htmlize ()
  (use-package htmlize
    :defer t
    ))

;; (defun daniel-org/init-org-gcal ()
;;   (use-package org-gcal
;;     :config
;;     (progn
;;       (setq org-gcal-client-id "68188842858-jcn9tafjlhbo4khh52a4iknncc1a5ufg.apps.googleusercontent.com"
;;             org-gcal-client-secret "iIaIHAJxpt40JGag3uQXe9AQ"
;;             org-gcal-file-alist '(("forhim185@gmail.com" .  "~/Dropbox/org/work.org")
;;                                   ("u7feg96tj7rf2pd7ekqt7a7u8c@group.calendar.google.com" .  "~/Dropbox/org/organizer.org")))
;;       )
;;     ))

(defun daniel-org/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun daniel-org/init-org ()
  (use-package org
    :bind (("C-c s" . org-store-link)
           ("C-c A" . org-agenda)
           ("C-c c" . org-capture))
    :mode ("\\.org$" . org-mode)
    :init
    (progn
      (define-key org-mode-map (kbd "C-c C-v" ) 'markdown-preview-file-with-marked)

      (require 'ob-tangle)
      (setq org-ditaa-jar-path "~/Dropbox/Apps/spacemacs/private/bin/ditaa0_9.jar")
      (setq org-plantuml-jar-path "~/Dropbox/Apps/spacemacs/private/bin/plantuml.jar")

      (defun bh/display-inline-images ()
        (condition-case nil
            (org-display-inline-images)
          (error nil)))
      (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
      (setq org-babel-results-keyword "results")

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

      (setq org-log-done t)
      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
      (setq org-startup-indented t)

      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

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

      ;; Remap org-mode meta keys for convenience
      (mapcar (lambda (state)
                (evil-declare-key state org-mode-map
                  (kbd "M-l") 'org-metaright
                  (kbd "M-h") 'org-metaleft
                  (kbd "M-k") 'org-metaup
                  (kbd "M-j") 'org-metadown))
              '(normal insert))

      (evil-leader/set-key-for-mode 'org-mode
        "c" 'org-archive-subtree-add-inherited-tags
        "m'" 'org-edit-special
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort
        "mI" 'org-clock-in

        (if dotspacemacs-major-mode-leader-key
            (concat "m" dotspacemacs-major-mode-leader-key)
          "m,") 'org-ctrl-c-ctrl-c
          "mj" 'helm-org-in-buffer-headings
          "mn" 'org-narrow-to-subtree
          "mw" 'widen
          "mO" 'org-clock-out
          "mq" 'org-clock-cancel
          "mR" 'org-refile
          "ms" 'org-schedule

          "mci" 'org-clock-in
          "mco" 'org-clock-out
          "mcq" 'org-clock-cancel

          ;; insertion of common elements
          "mil" 'org-insert-link
          "mif" 'org-footnote-new
          "mik" 'spacemacs/insert-keybinding-org
          "mih" 'org-insert-heading-after-current
          "miH" 'org-insert-heading

          ;; toggle
          "mtl" 'org-toggle-link-display
          "mti" 'org-toggle-inline-image
          "mtt" 'org-toggle-item
          "mto" 'org-toggle-ordered-property
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
      (use-package org-install)
      (use-package ox)
      (use-package org-archive)
      (require 'org-indent)
      (require 'ox-publish)
      (setq org-publish-project-alist
            '(("org-tasks"
               :base-directory "~/Dropbox/org/todo"
               :base-extension "org"
               :publishing-directory "~/Save To Evernote"
               :recursive t
               :publishing-function org-html-publish-to-html
               :headline-levels 4             ; Just the default for this project.
               :auto-preamble t)))

      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))

      (setq org-goto-interface 'outline
            org-goto-max-level 10)
      (setq org-startup-folded nil)
      (setq org-cycle-include-plain-lists 'integrate)
      (define-key org-mode-map (kbd "M-s-p") 'daniel/insert-link-org-link-with-title)
      (defvar daniel/org-basic-task-template "* TODO %^{Task} %^g
SCHEDULED: %^t
:PROPERTIES:
:Effort: %^{effort|0:30|1:00|2:00|4:00|1day|2day}
:END:
%?
" "Basic task data")

      (setq org-capture-templates
            `(("r" "Repo Todso" entry
               (file+headline "~/Dropbox/org/todo/repo_todo.org" "Repo Todos")
               "* TODO  %?\t\t\t%T\n %i\n Link: %l\n")
              ("t" "Tasks" entry
               (file+headline "~/Dropbox/org/todo/organizer.org" "Tasks")
               ,daniel/org-basic-task-template)
              ("q" "Quick task" entry
               (file+headline "~/Dropbox/org/todo/organizer.org" "Quick Tasks")
               "* TODO %^{Task}"
               :immediate-finish t)
              ("w" "Work task" entry
               (file+headline "~/Dropbox/org/todo/work.org" "Work Tasks")
               ,daniel/org-basic-task-template)
              ("m" "TODO from Mail" entry (file+headline "~/Dropbox/org/todo/email.org" "Inbox")
               "* TODO %?, Link: %a")
              ("n" "Notes" entry
               (file+datetree "~/Dropbox/org/todo/notes.org")
               "* %^{Title} %^g
Added: %T")))

      (defun hs/replace ()
        (interactive)
        (goto-char 1)
        (replace-string "INBOX" "Archive"))
      (add-hook 'org-capture-prepare-finalize-hook 'hs/replace)

      (require 'org-capture)
      (setq org-reverse-note-order t)
      (setq org-refile-use-outline-path nil)
      (setq org-refile-allow-creating-parent-nodes 'confirm)
      (setq org-refile-use-cache nil)
      (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
      (setq org-blank-before-new-entry nil)

      (setq org-todo-keywords
            '((sequence
               "TODO(t)"
               "STARTED(s)"
               "WAITING(w)"
               "MEMO(m)"
               "SOMEDAY(.)" "|" "DONE(d)" "CANCELLED(c)")))

      (setq org-todo-keyword-faces
            '(("TODO" . (:foreground "red" :weight bold))
              ("DONE" . (:foreground "green" :weight bold))
              ("STARTED" . (:foreground "cyan" :weight bold))
              ("CANCELLED" . (:foreground "dark magenta" :weight bold))
              ("MEMO" . (:foreground "blue" :weight bold))
              ("WAITING" . (:foreground "purple" :weight bold))
              ("SOMEDAY" . (:foreground "orange" :weight bold))))

      (setq org-tags-exclude-from-inheritance '("project"))
      (setq org-tag-alist '(("@work" . ?w)
                            ("@home" . ?h)
                            ("@steve" . ?s)
                            ("@buy" . ?b)
                            ("@personal" . ?p)
                            ("@errands" . ?e)
                            ("@church" . ?c)
                            ("@coding" . ?c)
                            ("@phone" . ?P)
                            ("@reading" . ?r)
                            ("@lowenergy" . ?0)
                            ("@highenergy" . ?1)))


      (setq org-log-into-drawer "LOGBOOK")
      (setq org-clock-into-drawer 1)

      (setq org-agenda-files
            (delq nil
                  (mapcar (lambda (x) (and (file-exists-p x) x))
                          '("~/Dropbox/org/todo/organizer.org"
                            "~/Dropbox/org/todo/notes.org"
                            "~/Dropbox/org/todo/repo_todo.org"
                            "~/Dropbox/org/todo/email.org"
                            "~/Dropbox/org/todo/work.org"))))

      (setq org-agenda-span 7)
      (setq org-agenda-sticky nil)
      (setq org-agenda-inhibit-startup t)
      (setq org-agenda-use-tag-inheritance nil)
      (setq org-agenda-show-log t)
      (setq org-agenda-skip-scheduled-if-done t)
      (setq org-agenda-skip-deadline-if-done t)
      (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
      (setq org-agenda-time-grid
            '((daily today require-timed)
              "----------------"
              (0800 1000 1200 1400 1600 1800)))
      (setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

      (setq org-agenda-start-on-weekday 6)

      (defvar sacha/org-agenda-contexts
        '((tags-todo "+@steve")
          (tags-todo "+@work")
          (tags-todo "+@coding")
          (tags-todo "+@buy")
          (tags-todo "+@church")
          (tags-todo "+@personal")
          (tags-todo "+@reading")
          (tags-todo "+@computer")
          (tags-todo "+@home")
          (tags-todo "+@errands"))
        "Usual list of contexts.")

      (setq org-agenda-exporter-settings
            '((ps-number-of-columns 2)
              (ps-landscape-mode t)
              (org-agenda-add-entry-text-maxlines 5)
              (htmlize-output-type 'css)))

      (require 'ox-latex)
      ;; 'djcb-org-article' for export org documents to the LaTex 'article', using
      ;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
      (unless (boundp 'org-latex-classes) (setq org-latex-classes nil))

      (setq org-latex-classes
            (cons '("org-article"
                    "\\documentclass[11pt,a4paper]{article}
      \\usepackage[T1]{fontenc}
      \\usepackage{fontspec}
      \\usepackage{graphicx}
      \\defaultfontfeatures{Mapping=tex-text}
      \\setromanfont{Arial}
      \\setromanfont [BoldFont={Arial Bold},
                      ItalicFont={Arial Basic Italic}]{Arial Basic}
      \\setsansfont{Helvetica}
      \\setmonofont[Scale=1.0]{Consolas}
      \\usepackage{geometry}
      \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                  marginparsep=7pt, marginparwidth=.6in}
      \\pagestyle{empty}
      \\title{}
            [NO-DEFAULT-PACKAGES]
            [NO-PACKAGES]"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                  org-latex-classes))


      (setq org-latex-to-pdf-process '("tex --pdf --clean --verbose --batch %f"))

      (setq org-agenda-custom-commands
            `(("T" tags-todo "TODO=\"TODO\"-goal-routine-SCHEDULED={.+}")
              ("w" todo ""
               ((org-agenda-files '("~/Dropbox/org/todo/work.org"))))
              ("m" todo ""
               ((org-agenda-files '("~/Dropbox/org/todo/mail.org"))))
              ("n" todo ""
               ((org-agenda-files '("~/Dropbox/org/todo/note.org"))))
              ("o" todo ""
               ((org-agenda-files '("~/Dropbox/org/todo/organizer.org"))))
              ("c" todo ""
               ((org-agenda-prefix-format "")
                (org-agenda-cmp-user-defined 'sacha/org-sort-agenda-items-todo)
                (org-agenda-view-columns-initially t)
                ))
              ;; Weekly review
              ("w" "Weekly review" agenda ""
               ((org-agenda-span 7)
                (org-agenda-log-mode 1)))
              ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
              ("gc" "Coding" tags-todo "@coding"
               ((org-agenda-view-columns-initially t)))
              ("gh" "Home" tags-todo "@home"
               ((org-agenda-view-columns-initially t)))
              ("0" "Top 3 by context"
               ,sacha/org-agenda-contexts
               ((org-agenda-sorting-strategy '(priority-up effort-down))
                (sacha/org-agenda-limit-items 3)))
              (")" "All by context"
               ,sacha/org-agenda-contexts
               ((org-agenda-sorting-strategy '(priority-down effort-down))
                (sacha/org-agenda-limit-items nil)))
              ("9" "Unscheduled top 3 by context"
               ,sacha/org-agenda-contexts
               ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
                (org-agenda-sorting-strategy '(priority-down effort-down))
                (sacha/org-agenda-limit-items 3)))
              ("(" "All unscheduled by context"
               ,sacha/org-agenda-contexts
               ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
                (org-agenda-sorting-strategy '(priority-down effort-down))
                ))
              ("d" "Timeline for today" ((agenda "" ))
               ((org-agenda-ndays 1)
                (org-agenda-show-log t)
                (org-agenda-log-mode-items '(clock closed))
                (org-agenda-clockreport-mode t)
                (org-agenda-entry-types '())))
              ("." "Waiting for" todo "WAITING")
              ("U" "Unscheduled tasks outside projects" tags-todo "-project"
               ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
                (org-tags-exclude-from-inheritance nil)
                (org-agenda-view-columns-initially t)
                (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
                (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down))))
              ("P" "By priority"
               ((tags-todo "+PRIORITY=\"A\"")
                (tags-todo "+PRIORITY=\"B\"")
                (tags-todo "+PRIORITY=\"\"")
                (tags-todo "+PRIORITY=\"C\""))
               ((org-agenda-prefix-format "%-10c %-10T %e ")
                (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
              ("S" tags-todo "TODO=\"STARTED\"")
              ("2" "List projects with tasks" sacha/org-agenda-projects-and-tasks
               "+PROJECT"
               ((sacha/org-agenda-limit-items 3)))))

      (setq org-agenda-sorting-strategy
            '((agenda time-up priority-down tag-up effort-up category-keep)
              (todo user-defined-up todo-state-up priority-down effort-up)
              (tags user-defined-up)
              (search category-keep)))
      (setq org-agenda-cmp-user-defined 'sacha/org-sort-agenda-items-user-defined)
      (setq org-export-with-section-numbers nil)
      (setq org-html-include-timestamps nil)
      (setq org-attach-store-link-p 'attached)
      (setq org-attach-auto-tag nil)
      (setq org-enforce-todo-dependencies t)
      (setq org-track-ordered-property-with-tag t)
      (setq org-agenda-dim-blocked-tasks t)

      ;; Use org.css from the :wq website for export document stylesheets
      (setq org-html-head-extra
            "<link rel=\"stylesheet\" href=\"http://dakrone.github.io/org.css\" type=\"text/css\" />")
      (setq org-html-head-include-default-style nil))))

(defun daniel-org/init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init
    (progn
      (add-hook 'org-mode-hook 'org-bullets-mode)
      (setq org-bullets-bullet-list
        '(;;; Large
          "◉"
          "○"
          "◆"
          "◇"
          "•"
          ))
      )))

(defun daniel-org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (evil-leader/set-key-for-mode 'org-mode
        "mp" 'org-pomodoro))))

(defun danielorg/init-toc-org ()
  (use-package toc-org
    :init
    (add-hook 'org-mode-hook 'toc-org-enable)))
