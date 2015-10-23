;;; packages.el --- daniel-email Layer packages File for Spacemacs
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
(setq daniel-email-packages
      '(
        gnus
        bbdb
        ))

;; List of packages to exclude.
(setq daniel-email-excluded-packages '())

;; For each package, define a function daniel-email/init-<package-name>
;;
;; (defun daniel-email/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun daniel-email/gnus ()
  (progn
    (require 'gnus)
    (setq mm-text-html-renderer 'w3m)
    (setq gnus-use-cache t)
    (setq gnus-select-method '(nnml ""))
    (setq gnus-select-method
          '(nnimap "gmail"
                   (nnimap-address "imap.gmail.com")
                   (nnimap-server-port 993)
                   (nnimap-stream ssl)))
    ;; (setq gnus-secondary-select-methods
    ;;       '((nnimap "private"
    ;;                 (nnimap-address "imap.gmail.com")
    ;;                 (nnimap-server-port 993)
    ;;                 (nnimap-stream ssl)
    ;;                 (nnir-search-engine imap)
    ;;                 (nnml-directory "~/Mail.private/")
    ;;                 (nnimap-authinfo-file "~/.authinfo"))
    ;;         (nnimap "work"
    ;;                 (nnimap-address "imap.gmail.com")
    ;;                 (nnimap-server-port 993)
    ;;                 (nnimap-stream ssl)
    ;;                 (nnir-search-engine imap)
    ;;                 (nnml-directory "~/Mail.work/")
    ;;                 (nnimap-authinfo-file "~/.authinfo"))))

    (setq gnus-thread-sort-functions
          '(
            (not gnus-thread-sort-by-date)
            (not gnus-thread-sort-by-number)
            ))
    (setq gnus-use-cache t)
    (setq gnus-use-adaptive-scoring t)
    (setq gnus-save-score t)
    (add-hook 'mail-citation-hook 'sc-cite-original)
    (add-hook 'message-sent-hook 'gnus-score-followup-article)
    (add-hook 'message-sent-hook 'gnus-score-followup-thread)
    (defvar gnus-default-adaptive-score-alist
      '((gnus-kill-file-mark (from -10))
        (gnus-unread-mark)
        (gnus-read-mark (from 10) (subject 30))
        (gnus-catchup-mark (subject -10))
        (gnus-killed-mark (from -1) (subject -30))
        (gnus-del-mark (from -2) (subject -15))
        (gnus-ticked-mark (from 10))
        (gnus-dormant-mark (from 5))))

    (setq-default
     gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
     gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

    (setq gnus-summary-line-format
          (concat
           "%0{%U%R%z%}"
           "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
           "  "
           "%4{%-20,20f%}"               ;; name
           "  "
           "%3{│%}"
           " "
           "%1{%B%}"
           "%s\n"))
    (setq gnus-summary-display-arrow t)
    (setq gnus-sum-thread-tree-indent "  ")
    (setq gnus-sum-thread-tree-root "") ;; "● ")
    (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
    (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
    (setq gnus-sum-thread-tree-vertical        "│")
    (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
    (setq gnus-sum-thread-tree-single-leaf     "╰─► ")
    ;; (setq gnus-parameters
    ;;       '(("nnimap work:INBOX"
    ;;          (display . all)
    ;;          (posting-style
    ;;           (name "IBS | Daniel")
    ;;           (address "danielc@ibsglobalweb.com")
    ;;           (organization "IBS")
    ;;           (signature-file "~/.signature-work"))
    ;;          (expiry-target . delete))

    ;;         ("nnimap work:[Gmail]/.*"
    ;;          (display . all)
    ;;          (posting-style
    ;;           (name "IBS | Daniel")
    ;;           (address "forhim185@gmail.com")
    ;;           (organization "IBS")
    ;;           (signature-file "~/.signature-work"))
    ;;          (expiry-wait . never))

    ;;         ("nnimap private:(INBOX|lists..*)"
    ;;          (display . all)
    ;;          (posting-style
    ;;           (name "Priavate | Daniel")
    ;;           (address "forhim185@gmail.com")
    ;;           (signature-file "~/.signature-private"))
    ;;          (expiry-target . delete))

    ;;         ("nnimap private:[Gmail]/.*"
    ;;          (display . all)
    ;;          (posting-style
    ;;           (name "Private | Daniel")
    ;;           (address "forhim185@gmail.com")
    ;;           (signature-file "~/.signature-private"))
    ;;          (expiry-wait . never))))

    (setq message-send-mail-function 'message-send-mail-with-sendmail)
    (setq sendmail-program "/usr/local/bin/msmtp")
    (setq gnus-permanently-visible-groups ".*")
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
    (setq gnus-thread-hide-subtree t)
    (setq gnus-thread-ignore-subject t)

    (setq user-full-name "Hanmoi Daniel Choi"
          user-mail-address "forhim185@gmail.com")
    (setq gnus-use-correct-string-widths nil)
    ))

(defun daniel-email/init-bbdb ()
  (use-package bbdb
    :init
    (progn
      (bbdb-initialize 'message 'gnus 'sendmail)
      (setq bbdb-file "~/bbdb.db")
      (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
      (setq bbdb/mail-auto-create-p t
            bbdb/news-auto-create-p t)
      (defvar bbdb-time-internal-format "%Y-%m-%d"
        "The internal date format.")
      (defun bbdb-timestamp-hook (record)
        "For use as a `bbdb-change-hook'; maintains a notes-field called `timestamp'
    for the given record which contains the time when it was last modified.  If
    there is such a field there already, it is changed, otherwise it is added."
        (bbdb-record-putprop record 'timestamp (format-time-string
                                                bbdb-time-internal-format
                                                (current-time))))
      (add-hook 'message-mode-hook
                '(lambda ()
                   (flyspell-mode t)
                   (local-set-key "<TAB>" 'bbdb-complete-name)))

      )
    ))
