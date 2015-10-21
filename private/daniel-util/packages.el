;;; packages.el --- daniel-util Layer packages File for Spacemacs
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
(setq daniel-util-packages
      '(
        ;; package names go here
        vlf
        find-file-in-project
        tabbar-ruler
        helm-ls-git
        ))

;; List of packages to exclude.
(setq daniel-util-excluded-packages '())

;; For each package, define a function daniel-util/init-<package-name>
;;
;; (defun daniel-util/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun daniel-util/init-vlf ()
  "Initialize my package"
  (use-package vlf
    :defer t
    ))

(defun daniel-util/init-find-file-in-project ()
  "Initialize my package"
  (use-package find-file-in-project
    ))

(defun daniel-util/init-tabbar-ruler ()
  "Initialize my package"
  (use-package tabbar-ruler
    :init
    (progn
      (defvar tabbar-prefix-map nil)
      (when (require 'tabbar-ruler nil t)
        (setq tabbar-buffer-list-function
              (lambda ()
                (remove-if
                 (lambda (buffer)
                   (let ((bn (buffer-name buffer)))
                     (and
                      (find (aref bn 0) " *")
                      (and
                       (> (length bn) 7)
                       (not (string= "*magit:" (substring bn 0 7)))
                       (not (string= "*prodigy" (substring bn 0 8)))
                       ))))
                 (buffer-list))))
        (tabbar-mode ))

      (global-set-key (kbd "<f9>") 'tabbar-mode)
      (setq tabbar-ruler-global-tabbar t)

      ;; Set nil
      (define-key tabbar-mode-map (kbd "C-x <left>") 'tabbar-backward-tab)
      (define-key tabbar-mode-map (kbd "C-x <right>") 'tabbar-forward-tab)
      (define-key tabbar-mode-map (kbd "C-x <down>") 'tabbar-backward-group)
      (define-key tabbar-mode-map (kbd "C-x <up>") 'tabbar-forward-group)

      ;; Add a buffer modification state indicator in the tab label, and place a
      ;; space around the label to make it looks less crowd.
      (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
        (setq ad-return-value
              (if (and (buffer-modified-p (tabbar-tab-value tab))
                       (buffer-file-name (tabbar-tab-value tab)))
                  (concat " + " (concat ad-return-value " "))
                (concat " " (concat ad-return-value " ")))))

      (setq tabbar-use-images nil)
      (tabbar-ruler-group-by-projectile-project))))

(defun daniel-util/init-helm-ls-git ()
  "Initialize my package"
  (use-package helm-ls-git
    :defer t
    :bind ("C-x C-d" . helm-browse-project)))
