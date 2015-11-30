
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
        gmail2bbdb
        ))

;; List of packages to exclude.
(setq daniel-email-excluded-packages '())

(defun daniel-email/init-gmail2bbdb ()
  (use-package gmail2bbdb
    :init
    (progn
      (setq gmail2bbdb-bbdb-file "~/Dropbox/Apps/bbdb.db"))))

(defun daniel-email/init-gnus ()
  "Initialize my package"
  (use-package gnus
    :defer t
    :commands gnus
    :init
    (evil-leader/set-key "ag" 'gnus)
    :config
    (progn
      (setq gnus-select-method '(nnnil ""))

                                        ; Use topics per default:
      (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

      (setq gnus-visible-headers
            "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

    ;;; Show the article headers in this order.
      (setq gnus-sorted-header-list
            '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
              "^Subject:" "^Date:" "^Gnus"))
      (defun my-gnus-group-list-subscribed-groups ()
        "List all subscribed groups with or without un-read messages"
        (interactive)
        (gnus-group-list-all-groups 5)
        )
      (add-hook 'gnus-group-mode-hook
                ;; list all the subscribed groups even they contain zero un-read messages
                (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups ))
                )
      (setq gnus-secondary-select-methods '((nnml "")))
      (setq mm-text-html-renderer 'w3m)
      (setq gnus-use-cache t)
      (setq gnus-secondary-select-methods
            '((nnimap "private"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnml-directory "~/Dropbox/Mail.private/")
                      (nnimap-authinfo-file "~/.authinfo"))
              (nnimap "work"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnml-directory "~/Dropbox/Mail.work/")
                      (nnimap-authinfo-file "~/.authinfo"))))

      (setq gnus-thread-sort-functions
            '(
              (not gnus-thread-sort-by-date)
              (not gnus-thread-sort-by-number)))

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
      (setq gnus-parameters
            '(("nnimap work:(INBOX|JIRA"
               (display . all)
               (posting-style
                (name "IBS | Daniel")
                (address "danielc@ibsglobalweb.com")
                (organization "IBS")
                (signature-file "~/.signature-work"))
               (expiry-target . delete))

              ("nnimap work:[Gmail]/(All Mail|Sent Mail)"
               (display . all)
               (posting-style
                (name "IBS | Daniel")
                (address "forhim185@gmail.com")
                (organization "IBS")
                (signature-file "~/.signature-work"))
               (expiry-wait . never))

              ("nnimap private:(INBOX|Bill)"
               (display . all)
               (posting-style
                (name "Priavate | Daniel")
                (address "forhim185@gmail.com")
                (signature-file "~/.signature-private"))
               (expiry-target . delete))

              ("nnimap private:[Gmail]/(All Mail|Sent Mail|)"
               (display . all)
               (posting-style
                (name "Private | Daniel")
                (address "forhim185@gmail.com")
                (signature-file "~/.signature-private"))
               (expiry-wait . never))))

      (setq message-send-mail-function 'message-send-mail-with-sendmail)
      (setq sendmail-program "/usr/local/bin/msmtp")
      (setq gnus-permanently-visible-groups ".*")
      (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
      (setq gnus-thread-hide-subtree t)
      (setq gnus-thread-ignore-subject t)

      (setq user-full-name "Hanmoi Daniel Choi"
            user-mail-address "forhim185@gmail.com")
      (setq gnus-use-correct-string-widths nil)

      (require 'browse-url)
      (require 'nnrss)
      (defun spacemacs/browse-nnrss-url (arg)
        "Open RSS Article directy in the browser"
        (interactive "p")
        (let ((url (assq nnrss-url-field
                         (mail-header-extra
                          (gnus-data-header
                           (assq (gnus-summary-article-number)
                                 gnus-newsgroup-data))))))
          (if url
              (progn
                (browse-url (cdr url))
                (gnus-summary-mark-as-read-forward 1))
            (gnus-summary-scroll-up arg))))
      (add-to-list 'nnmail-extra-headers nnrss-url-field))))

(defun daniel-email/init-bbdb ()
  (use-package bbdb
    :init
    (progn
      (setq bbdb-file "~/Dropbox/Apps/bbdb.db")
      (bbdb-initialize 'message 'gnus 'sendmail)
      (setq
       bbdb-offer-save 1                        ;; 1 means save-without-asking
       bbdb-use-pop-up t                        ;; allow popups for addresses
       bbdb-electric-p t                        ;; be disposable with SPC
       bbdb-popup-target-lines  1               ;; very small
       bbdb-dwim-net-address-allow-redundancy t ;; always use full name
       bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
       bbdb-always-add-address t                ;; add new addresses to existing...
       bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
       bbdb-completion-type nil                 ;; complete on anything
       bbdb-complete-name-allow-cycling t       ;; cycle through matches
       bbbd-message-caching-enabled t           ;; be fast
       bbdb-use-alternate-names t               ;; use AKA
       bbdb-elided-display t                    ;; single-line addresses
       ;; auto-create addresses from mail
       bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)
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
                    (local-set-key "<TAB>" 'bbdb-complete-name))))))
