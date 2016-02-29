
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
      (global-set-key (kbd "s-b") 'browse-url-at-point)
      (evil-leader/set-key "ab" 'w3m)
      (setq w3m-home-page "http://www.google.com")
      (standard-display-ascii ?\225 [?+])
      (standard-display-ascii ?\227 [?-])
      (standard-display-ascii ?\222 [?'])
      (setq
       w3m-use-cookies t
       w3m-default-desplay-inline-images t
       w3m-use-filter t
       w3m-coding-system 'utf-8
       w3m-output-coding-system 'utf-8
       w3m-input-coding-system 'utf-8
       w3m-terminal-coding-system 'utf-8)

      )
    :config
    (progn
      (setq browse-url-browser-function 'w3m-goto-url-new-session)
      ;; optional keyboard short-cut
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
      (define-key w3m-mode-map [mouse-2] 'w3m-mouse-view-this-url-new-session)

      (defun w3m-get-buffer-with-org-style ()
        "Get current buffer content with `org-mode' style.
This function will encode `link-title' and `link-location' with `org-make-link-string'.
And move buffer content to lastest of kill ring.
So you can yank in `org-mode' buffer to get `org-mode' style content."
        (interactive)
        (let (transform-start
              transform-end
              return-content
              link-location
              link-title
              temp-position
              out-bound)
          (if mark-active
              (progn
                (setq transform-start (region-beginning))
                (setq transform-end (region-end))
                (deactivate-mark))
            (setq transform-start (point-min))
            (setq transform-end (point-max)))
          (message "Start transform link to `org-mode' style, please wait...")
          (save-excursion
            (goto-char transform-start)
            (while (and (not out-bound)             ;not out of transform bound
                        (not (w3m-no-next-link-p))) ;not have next link in current buffer
              ;; store current point before jump next anchor
              (setq temp-position (point))
              ;; move to next anchor when current point is not at anchor
              (or (w3m-anchor (point)) (w3m-get-next-link-start))
              (if (<= (point) transform-end)  ;if current point is not out of transform bound
                  (progn
                    ;; get content between two links.
                    (if (> (point) temp-position)
                        (setq return-content (concat return-content (buffer-substring temp-position (point)))))
                    ;; get link location at current point.
                    (setq link-location (w3m-anchor (point)))
                    ;; get link title at current point.
                    (setq link-title (buffer-substring (point) (w3m-get-anchor-end)))
                    ;; concat `org-mode' style url to `return-content'.
                    (setq return-content (concat return-content (org-make-link-string link-location link-title))))
                (goto-char temp-position)     ;reset point before jump next anchor
                (setq out-bound t)            ;for break out `while' loop
                ))
            ;; concat rest context of current buffer
            (if (< (point) transform-end)
                (setq return-content (concat return-content (buffer-substring (point) transform-end))))
            (kill-new return-content)
            (message "Transform link completed. You can get it from lastest kill ring."))))

      (defun w3m-get-anchor-start ()
        "Move and return `point' for thst start of the current anchor."
        (interactive)
        (goto-char (or (previous-single-property-change (point) 'w3m-anchor-sequence) ;get start position of anchor
                       (point)))                                                      ;or current point
        (point))

      (defun w3m-get-anchor-end ()
        "Move and return `point' after the end of current anchor."
        (interactive)
        (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence) ;get end position of anchor
                       (point)))                                                  ;or current point
        (point))

      (defun w3m-get-next-link-start ()
        "Move and return `point' for that start of the current link."
        (interactive)
        (catch 'reach
          (while (next-single-property-change (point) 'w3m-anchor-sequence) ;jump to next anchor
            (goto-char (next-single-property-change (point) 'w3m-anchor-sequence))
            (when (w3m-anchor (point))        ;return point when current is valid link
              (throw 'reach nil))))
        (point))

      (defun w3m-get-prev-link-start ()
        "Move and return `point' for that end of the current link."
        (interactive)
        (catch 'reach
          (while (previous-single-property-change (point) 'w3m-anchor-sequence) ;jump to previous anchor
            (goto-char (previous-single-property-change (point) 'w3m-anchor-sequence))
            (when (w3m-anchor (point))        ;return point when current is valid link
              (throw 'reach nil))))
        (point))

      (defun w3m-no-next-link-p ()
        "Return t if no next link after cursor.
Otherwise, return nil."
        (save-excursion
          (equal (point) (w3m-get-next-link-start))))

      (defun w3m-no-prev-link-p ()
        "Return t if no prevoius link after cursor.
Otherwise, return nil."
        (save-excursion
          (equal (point) (w3m-get-prev-link-start))))
      (define-key w3m-minor-mode-map (kbd "M-w") 'w3m-get-buffer-with-org-style))))
