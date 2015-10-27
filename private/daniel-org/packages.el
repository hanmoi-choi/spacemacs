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
;;; License: GPLv3

(setq daniel-org-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    ;; evil-org
    gnuplot
    htmlize
    ;; org is installed by `org-plus-contrib'
    (org :location built-in)
    (org-plus-contrib :step pre)
    org-bullets
    ;; org-mime is installed by `org-plus-contrib'
    (org-mime :location built-in)
    org-pomodoro
    org-present
    toc-org))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun daniel-org/post-init-company ()
    (spacemacs|add-company-hook org-mode)
    (push 'company-capf company-backends-org-mode))
  (defun daniel-org/post-init-company-emoji ()
    (push 'company-emoji company-backends-org-mode)))

(defun daniel-org/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'org-mode-hook 'spacemacs/delay-emoji-cheat-sheet-hook))

(defun daniel-org/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'org-mode
        "mC" 'evil-org-recompute-clocks

        ;; evil-org binds these keys, so we bind them back to their original
        ;; value
        "t" (lookup-key evil-leader--default-map "t")
        "a" (lookup-key evil-leader--default-map "a")
        "b" (lookup-key evil-leader--default-map "b")
        "c" (lookup-key evil-leader--default-map "c")
        "l" (lookup-key evil-leader--default-map "l")
        "o" (lookup-key evil-leader--default-map "o"))
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun daniel-org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (evil-leader/set-key-for-mode 'org-mode
            "mtp" 'org-plot/gnuplot)))

;; dummy init function to force installation of `org-plus-contrib'
(defun daniel-org/init-org-plus-contrib ())

(defun daniel-org/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :bind ("C-c c" . org-capture)
    :defer t
    :init
    (progn
      (setq org-clock-persist-file
            (concat spacemacs-cache-directory "org-clock-save.el")
            org-log-done t
            org-startup-with-inline-images t
            org-src-fontify-natively t)

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
      (setq org-startup-indented t)
      (let ((dir (configuration-layer/get-layer-property 'daniel-org :dir)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
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
      (evil-leader/set-key-for-mode 'org-mode
        "m'" 'org-edit-special
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort
        "m:" 'org-set-tags

        "ma" 'org-agenda
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

        ;; Change between TODO sets
        "m C-S-l" 'org-shiftcontrolright
        "m C-S-h" 'org-shiftcontrolleft
        "m C-S-j" 'org-shiftcontroldown
        "m C-S-k" 'org-shiftcontrolup

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
          "mq" 'org-clock-cancel
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
      ;; setup org directory
      (unless (file-exists-p org-directory)
        (make-directory org-directory))
      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))

      (require 'org-indent)
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '")) 'org-edit-src-exit)

      (evil-leader/set-key
        "Cc" 'org-capture)

      (defun markdown-preview-file-with-marked ()
        "run Marked on the current file (convert it to markdown in advance if the file is *.org)."
        (interactive)
        (if (string= (file-name-extension buffer-file-name) "org")
            (org-md-export-to-markdown) nil)
        (shell-command
         (format "open -a /Applications/Marked\\ 2.app %s"
                 (shell-quote-argument
                  (concat (file-name-sans-extension buffer-file-name) ".md")))))

      (define-key org-mode-map (kbd "C-x C-v" ) 'markdown-preview-file-with-marked)
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
               (file+headline "~/Dropbox/org/todo/repo_todo.org" "Repo Task ")
               "* TODO  %?\t\t\t%T\n %i\n Link: %l\n")
              ("t" "Tasks" entry
               (file+headline "~/Dropbox/org/todo/organizer.org" "PRIVATE Task")
               ,daniel/org-basic-task-template)
              ("q" "Quick task" entry
               (file+headline "~/Dropbox/org/todo/organizer.org" "Quick Tasks")
               "* TODO %^{Task} %^g"
               :immediate-finish t)
              ("w" "Work task" entry
               (file+headline "~/Dropbox/org/todo/work.org" "IBSA Tasks")
               ,daniel/org-basic-task-template)
              ("m" "TODO from Mail" entry (file+headline "~/Dropbox/org/todo/email.org" "INBOX")
               "* TODO %? %^g\nLink: %a")
              ("n" "Notes" entry
               (file+datetree "~/Dropbox/org/todo/notes.org")
               "* %^{Title} %^g \nAdded: %T")))

      (defun daniel/org-level1-replace ()
        (interactive)
        (goto-char 1)
        (progn
          (replace-string "INBOX" "Archive")))

      (add-hook 'org-capture-prepare-finalize-hook 'daniel/org-level1-replace)
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
            '(("TODO" . (:foreground "#CC9393" :weight bold))
              ("DONE" . (:foreground "#7F9F7F" :weight bold))
              ("STARTED" . (:foreground "#93E0E3" :weight bold))
              ("CANCELLED" . (:foreground "#DC8CC3" :weight bold))
              ("MEMO" . (:foreground "#94BFF3" :weight bold))
              ("WAITING" . (:foreground "purple" :weight bold))
              ("SOMEDAY" . (:foreground "#656555" :weight bold))))

      (setq org-tags-exclude-from-inheritance '("project"))
      (setq org-tag-alist '(("@work" . ?w)
                            ("@home" . ?h)
                            ("@Steve" . ?s)
                            ("@Michael" . ?s)
                            ("@buy" . ?b)
                            ("@personal" . ?p)
                            ("@errands" . ?e)
                            ("@church" . ?c)
                            ("@coding" . ?c)
                            ("@phone" . ?P)
                            ("@reading" . ?r)
                            ("@fm_origination" . ?O)
                            ("@fm_insurance" . ?I)
                            ("@fm_admin" . ?A)
                            ("@JIRA" . ?J)
                            ("@fm_admin" . ?A)
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
              (0900 1100 1300 1500 1700 1900)))
      (setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")
      (setq org-agenda-start-on-weekday 6)

      (defvar sacha/org-agenda-contexts
        '((tags-todo "+@Steve")
          (tags-todo "+@Michael")
          (tags-todo "+@fm_origination")
          (tags-todo "+@fm_insurance")
          (tags-todo "+@fm_admin")
          (tags-todo "+@JIRA")
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
          "✚"
          "◉"
          "○"
          "◆"
          "◇"
          "•"
          )))))

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

(defun daniel-org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilify nil org-present-mode-keymap
               "h" 'org-present-prev
               "l" 'org-present-next
               "q" 'org-present-quit)
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-evilified-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write)
        (evil-normal-state))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

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
