
;;; packages.el --- daniel-web-browser Layer packages File for Spacemacs
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
(setq daniel-web-browser-packages
      '(
        w3m
        ))

;; List of packages to exclude.
(setq daniel-web-browser-excluded-packages '())

;; For each package, define a function daniel-web-browser/init-<package-name>
;;
;; (defun daniel-web-browser/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun daniel-web-browser/init-w3m ()
  (use-package w3m
    :init
    (progn
      (setq browse-url-browser-function 'w3m-browse-url)
      (setq w3m-default-desplay-inline-images t)
      (setq w3m-default-save-directory "~/download/")
      ;; optional keyboard short-cut
      (global-set-key "\C-xm" 'browse-url-at-point)
      (setq w3m-use-cookies t)
      (standard-display-ascii ?\225 [?+])
      (standard-display-ascii ?\227 [?-])
      (standard-display-ascii ?\222 [?'])
      (setq w3m-use-filter t)
      (setq w3m-coding-system 'utf-8
            w3m-output-coding-system 'utf-8
            w3m-input-coding-system 'utf-8
            w3m-terminal-coding-system 'utf-8
            w3m-default-display-inline-images t)
      ;; send all pages through one filter
      (setq w3m-filter-rules `(("\\`.+" w3m-filter-all)))

      (defun w3m-filter-all (url)
        (let ((list '(
                      ;; add more as you see fit!
                      ("&#187;" "&gt;&gt;")
                      ("&laquo class="comment">;" "&lt;")
                      ("&raquo class="comment">;" "&gt;")
                      ("&ouml class="comment">;" "o")
                      ("&#8230;" "...")
                      ("&#8216;" "'")
                      ("&#8217;" "'")
                      ("&rsquo class="comment">;" "'")
                      ("&lsquo class="comment">;" "'")
                      ("\u2019" "\'")
                      ("\u2018" "\'")
                      ("\u201c" "\"")
                      ("\u201d" "\"")
                      ("&rdquo class="comment">;" "\"")
                      ("&ldquo class="comment">;" "\"")
                      ("&#8220;" "\"")
                      ("&#8221;" "\"")
                      ("\u2013" "-")
                      ("\u2014" "-")
                      ("&#8211;" "-")
                      ("&#8212;" "-")
                      ("&ndash class="comment">;" "-")
                      ("&mdash class="comment">;" "-")
                      )))
          (while list
            (let ((pat (car (car list)))
                  (rep (car (cdr (car list)))))
              (goto-char (point-min))
              (while (search-forward pat nil t)
                (replace-match rep))
              (setq list (cdr list))))))
      )
    :config
    (progn
      (evil-leader/set-key
        "@h" 'hn
        "@w" 'wikipedia-search
        "@g" 'w3m-goto-url)
      (define-key w3m-mode-map (kbd "M-<right>") 'w3m-next-buffer)
      (define-key w3m-mode-map (kbd "M-<left>") 'w3m-previous-buffer)
      (define-key w3m-mode-map (kbd "V") 'helm-w3m-bookmarks)
      (define-key w3m-mode-map (kbd "M") 'w3m-view-url-with-browse-url)

      (setq w3m-home-page "http://www.google.com")
      (if (string= system-type "darwin")
          (setq process-connection-type nil))
      (defun w3m-new-tab ()
        (interactive)
        (w3m-copy-buffer nil nil nil t))
      (defun w3m-browse-url-new-tab (url &optional new-session)
        (interactive)
        (w3m-new-tab)
        (w3m-browse-url url))
      (setq browse-url-browser-function 'w3m-browse-url-new-tab)

      (defun w3m-copy-url-at-point ()
        (interactive)
        (let ((url (w3m-anchor)))
          (if (w3m-url-valid url)
              (kill-new (w3m-anchor))
            (message "No URL at point!"))))
      (add-hook 'w3m-mode-hook
                (lambda ()
                  (local-set-key "\M-W" 'w3m-copy-url-at-point)))
      (add-hook 'w3m-mode-hook (lambda () (define-key w3m-mode-map (kbd "C-x b") nil)))

      (defun hn ()
        (interactive)
        (browse-url "http://news.ycombinator.com"))

      ;;i need this often
      (defun wikipedia-search (search-term)
        "Search for SEARCH-TERM on wikipedia"
        (interactive
         (let ((term (if mark-active
                         (buffer-substring (region-beginning) (region-end))
                       (word-at-point))))
           (list
            (read-string
             (format "Wikipedia (%s):" term) nil nil term)))
         )
        (browse-url
         (concat
          "http://en.m.wikipedia.org/w/index.php?search="
          search-term))))))
