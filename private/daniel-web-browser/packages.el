
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
    :config
    (progn
      (setq browse-url-browser-function 'w3m-browse-url)
      (setq w3m-default-desplay-inline-images t)
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
            w3m-default-display-inline-images t))
      (evil-leader/set-key
        "@h" 'hn
        "@w" 'wikipedia-search
        "@g" 'w3m-goto-url)
      (define-key w3m-mode-map (kbd "M-<right>") 'w3m-next-buffer)
      (define-key w3m-mode-map (kbd "M-<left>") 'w3m-previous-buffer)
      (define-key w3m-mode-map (kbd "V") 'helm-w3m-bookmarks)
      (define-key w3m-mode-map (kbd "M") 'w3m-view-url-with-browse-url)

      (setq w3m-home-page "http://www.google.com")

      (setq browse-url-browser-function 'w3m-browse-url-new-tab)
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
          search-term)))))
