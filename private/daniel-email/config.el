;; Email Client
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "~/.emacs.d/vendor")

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; mu4e
(require 'mu4e)
(require 'org-mu4e)
(require 'mu4e-multi)

(eval-after-load 'evil-evilified-state
  '(progn
     (eval-after-load 'mu4e
      '(progn
         ;; use the standard bindings as a base
         (spacemacs|evilify-map mu4e-view-mode-map
           :mode mu4e-view-mode
           :bindings
           (kbd "/") 'mu4e-view-search-narrow
           (kbd "SPC") 'mu4e-view-scroll-up-or-next
           (kbd "J") 'mu4e-view-headers-next
           (kbd "K") 'mu4e-view-headers-prev
           (kbd "C") 'mu4e-multi-compose-new
           (kbd "o") 'mu4e-view-message
           (kbd "Q") 'mu4e-raw-view-quit-buffer)

         (spacemacs|evilify-map mu4e-main-mode-map
           :mode mu4e-main-mode
           :bindings
           (kbd "C") 'mu4e-multi-compose-new
           (kbd "J") 'mu4e~headers-jump-to-maildir
           (kbd "j") 'evil-next-line
           (kbd "RET") 'mu4e-view-message)

         (spacemacs|evilify-map mu4e-headers-mode-map
           :mode mu4e-headers-mode
           :bindings
           (kbd "/") 'mu4e-headers-search-narrow
           (kbd "J") 'mu4e~headers-jump-to-maildir
           (kbd "j") 'evil-next-line
           (kbd "C") 'mu4e-multi-compose-new
           (kbd "o") 'mu4e-view-message)
         ))))

(setq mu4e-view-show-images t)
(setq mu4e-maildir "~/Dropbox/Maildir")

(setq mu4e-multi-account-alist
      '(("personal"
         (user-mail-address . "forhim185@gmail.com")
         (mu4e-drafts-folder . "/personal/[Gmail].Drafts")
         (mu4e-refile-folder . "/personal/Archive")
         (mu4e-sent-folder . "/personal/[Gmail].Sent Mail")
         (mu4e-trash-folder . "/personal/[Gmail].Trash")
         )
        ("work"
         (user-mail-address . "danielc@ibsglobalweb.com")
         (mu4e-drafts-folder . "/work/[Gmail].Drafts")
         (mu4e-refile-folder . "/work/Archive")
         (mu4e-sent-folder . "/work/[Gmail].Sent Mail")
         (mu4e-trash-folder . "/work/[Gmail].Trash")
         )
        ))
(mu4e-multi-enable)
(global-set-key (kbd "s-n") 'mu4e-multi-compose-new)

;; Creates `mu4e-multi-mark-for-hold' command.
(mu4e-multi-make-mark-for-command mu4e-hold-folder)

;; Creates `mu4e-multi-mark-for-follow-up' command.
(mu4e-multi-make-mark-for-command mu4e-follow-up-folder)
(add-hook 'message-send-mail-hook 'mu4e-multi-smtpmail-set-msmtp-account)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-view-prefer-html t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-update-interval 120)

(setq mu4e-maildir-shortcuts
      '(
        ("/personal/INBOX" . ?p)
        ("/personal/BILL" . ?b)
        ("/work/INBOX" . ?w)
        ("/work/JIRA" . ?j)
        ))

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
(setq mu4e-html2text-command "textutil -stdin -format html --convert txt -stdout")
;; (setq mu4e-html2text-command "w3m -dump -graph -cols 120 -T text/html")

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; fetch mail every 10 mins
;; something about ourselves
(setq
   user-mail-address "danielc@ibsglobalweb.com"
   user-full-name  "Hanmoi Daniel Choi"
   mu4e-compose-signature
    (concat
      "Hanmoi Daniel Choi"))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(defun tv/message-mode-setup ()
  (setq fill-column 80)
  (turn-on-auto-fill)
  (epa-mail-mode 1))
(add-hook 'message-mode-hook 'tv/message-mode-setup)

;;; Default
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-view-show-addresses t)

;;; Html rendering
(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command (cond ((fboundp 'w3m)
                                    (lambda ()          ; Use emacs-w3m
                                      (w3m-region (point-min) (point-max))))
                                   ((executable-find "w3m")
                                    "w3m -T text/html") ; Use w3m shell-command
                                   (t (lambda ()        ; Use shr (slow)
                                        (let ((shr-color-visible-luminance-min 75)
                                              shr-width)
                                          (shr-render-region (point-min) (point-max)))))))

(setq mail-user-agent 'mu4e-user-agent)
(setq read-mail-command 'mu4e)
(define-key mu4e-main-mode-map "q" 'quit-window)
(define-key mu4e-main-mode-map "Q" 'mu4e-quit)

;;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-headers-skip-duplicates t)

;;; encryption
(define-key mu4e-view-mode-map [remap mu4e-view-verify-msg-popup] 'epa-mail-verify)

(setq mu4e-bookmarks
      '(("date:1w..now helm AND NOT flag:trashed" "Last 7 days helm messages" ?h)
        ("flag:unread AND NOT flag:trashed AND (maildir:/personal/INBOX OR maildir:/work/INBOX)" "Unread messages" ?u)
        ("date:today..now AND (maildir:/personal/INBOX OR maildir:/work/INBOX)" "Today's messages" ?t)
        ("date:1d..now AND (maildir:/personal/INBOX OR maildir:/work/INBOX)" "Yesterday and today messages" ?y)
        ("date:7d..now AND (maildir:/personal/INBOX OR maildir:/work/INBOX)" "Last 7 days" ?w)
        ("flag:attach" "Having Attachements" ?A)
        ("from:stevev@ibsglobalweb.com AND (maildir:/personal/INBOX OR maildir:/work/INBOX)" "From Steve" ?s)
        ))

(add-hook 'mu4e-compose-mode-hook 'tv/message-mode-setup) ; loaded from .gnus.el

;;; Use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;;; Save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/download")

;;; Updating
;;
;;
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap -q -u Basic")

;;; Automatic updates.
;(setq mu4e-update-interval 600)

;;; Make a full update all the
;; `tv/mu4e-max-number-update-before-toggling' mail retrievals.
(defvar tv/mu4e-counter 10) ; Ensure a full update on startup.
(defvar tv/mu4e-max-number-update-before-toggling 10)
(defvar tv/mu4e-get-mail-command-full "offlineimap -u Basic")
(defvar tv/mu4e-get-mail-command-quick "offlineimap -q -u Basic")
(defun tv/mu4e-update-mail-quick-or-full ()
  (if (>= tv/mu4e-counter
          tv/mu4e-max-number-update-before-toggling)
      (progn
        (setq mu4e-get-mail-command tv/mu4e-get-mail-command-full)
        (setq tv/mu4e-counter 0))
      (setq mu4e-get-mail-command tv/mu4e-get-mail-command-quick)
      (incf tv/mu4e-counter)))
(add-hook 'mu4e-update-pre-hook #'tv/mu4e-update-mail-quick-or-full)

;;; Attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;;; Allow queuing mails
(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir   "~/Dropbox/Maildir/queue/")

;;; View html message in firefox (type aV)
(add-to-list 'mu4e-view-actions
            '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;; Decorate mu main view
(defun mu4e-main-mode-font-lock-rules ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([a-zA-Z]\\{1,2\\}\\)\\]" nil t)
      (add-text-properties (match-beginning 1) (match-end 1) '(face font-lock-variable-name-face)))))
(add-hook 'mu4e-main-mode-hook 'mu4e-main-mode-font-lock-rules)

;;; Handle quoted text added with `message-mark-inserted-region' (`C-c M-m')
(add-hook 'mu4e-view-mode-hook 'mu4e-mark-region-code)

(defun tv/mu4e-browse-url ()
  (interactive)
  (browse-url (w3m-active-region-or-url-at-point)))
(define-key mu4e-view-mode-map (kbd "C-c C-c") 'tv/mu4e-browse-url)

(defadvice w3m-goto-next-anchor (before go-to-end-of-anchor activate)
  (when (w3m-anchor-sequence)
    (goto-char (next-single-property-change
                (point) 'w3m-anchor-sequence))))

(defadvice w3m-goto-previous-anchor (before go-to-end-of-anchor activate)
  (when (w3m-anchor-sequence)
    (goto-char (previous-single-property-change
                (point) 'w3m-anchor-sequence))))

(define-key mu4e-view-mode-map (kbd "C-i") 'w3m-next-anchor)
(define-key mu4e-view-mode-map (kbd "M-<tab>") 'w3m-previous-anchor)

;;; A simplified and more efficient version of `article-translate-strings'.
;;
;; Transform also in headers.
(defun mu4e~view-translate-strings (map)
  "Translate all string in the the article according to MAP.
MAP is an alist where the elements are on the form (\"from\" \"to\")."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (dolist (elem map)
        (let* ((key  (car elem))
               (from (if (characterp key) (string key) key))
               (to   (cdr elem)))
          (save-excursion
            (while (search-forward from nil t)
              (replace-match to))))))))

(defun mu4e-view-treat-dumbquotes ()
    "Translate M****s*** sm*rtq**t*s and other symbols into proper text.
Note that this function guesses whether a character is a sm*rtq**t* or
not, so it should only be used interactively.
Sm*rtq**t*s are M****s***'s unilateral extension to the
iso-8859-1 character map in an attempt to provide more quoting
characters.  If you see something like \\222 or \\264 where
you're expecting some kind of apostrophe or quotation mark, then
try this wash."
  (interactive)
  (with-current-buffer mu4e~view-buffer
    (mu4e~view-translate-strings
     '((128 . "EUR") (130 . ",") (131 . "f") (132 . ",,")
       (133 . "...") (139 . "<") (140 . "OE") (145 . "`")
       (146 . "'") (147 . "``") (148 . "\"") (149 . "*")
       (150 . "-") (151 . "--") (152 . "~") (153 . "(TM)")
       (155 . ">") (156 . "oe") (180 . "'")))))

;;; Same as `article-remove-cr' (W-c) but simplified and more efficient.
;;
;;; Show Smileys
(add-hook 'mu4e-view-mode-hook 'smiley-buffer)
(add-hook 'mu4e-compose-mode-hook 'company-mode)
(defalias 'org-mail 'org-mu4e-compose-org-mode)
