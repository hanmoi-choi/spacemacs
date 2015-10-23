;;; packages.el --- e2wm Layer packages File for Spacemacs
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
(setq e2wm-packages
      '(
        e2wm
        e2wm-bookmark
        e2wm-direx
        ))

;; List of packages to exclude.
(setq e2wm-excluded-packages '())

;; For each package, define a function e2wm/init-<package-name>
;;
;; (defun e2wm/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun e2wm/init-e2wm-bookmark ()
  (use-package e2wm-bookmark)
  )

(defun e2wm/init-e2wm-direx()
  (use-package e2wm-direx))

(defun e2wm/init-e2wm ()
  (use-package e2wm
    :defer t
    :init
    (progn
      ;; neotree plugin
      (setq e2wm:c-max-history-num 30)
      (setq e2wm:c-code-recipe
            '(| (:left-max-size 30)
                file
                (| (:right-max-size 30)
                   (- (:upper-size-ratio 0.7)
                      main sub)
                   (- (:upper-size-ratio 0.6)
                      history bookmarks))))
      (setq e2wm:c-code-winfo
            '((:name history :plugin history-list)
              (:name sub :buffer "*helm*" :default-hide t)
              (:name bookmarks :plugin bookmarks-list)
              (:name file :plugin dired)
              (:name main)))

      (setq e2wm:c-two-recipe
            '(- (:upper-size-ratio 0.7)
                (| left
                   (- (:upper-size-ratio 0.7)
                      right history))
                sub))

      (setq e2wm:c-two-winfo
            '((:name left )
              (:name right )
              (:name sub :buffer "*helm*" :default-hide t)
              (:name history :plugin history-list :default-hide nil)))

      (dolist (mode '(e2wm:def-plugin-history-list-mode
                      e2wm:def-plugin-bookmarks-list-mode
                      ))
        (add-to-list 'evil-emacs-state-modes mode))

      (defface e2wm:face-history-list-normal
        '((t :foreground "#DCCCC"))
        "Face for e2wm history list." :group 'e2wm)
      (defface e2wm:face-history-list-select1
        '((t :foreground "#DFAF8F"))
        "Face for e2wm history list." :group 'e2wm)
      (defface e2wm:face-history-list-select2
        '((t :foreground "#94BFF3" ))
        "Face for e2wm history list." :group 'e2wm)

      (defun toggle-e2wm ()
        (interactive)
        (if (bound-and-true-p e2wm:pst-minor-mode)
            (e2wm:stop-management)
          (e2wm:start-management)))
      (global-set-key (kbd "<f8>") 'toggle-e2wm))
    :config
    (progn
      (e2wm:add-keymap
       e2wm:pst-minor-mode-keymap
       '(("M-]"       . e2wm:pst-history-forward-command)
         ("M-["       . e2wm:pst-history-back-command)
         ) e2wm:prefix-key)

      (eval-after-load "widen-window"
        '(progn
           (defun e2wm:fix-widen-window-pre-start ()
             (defadvice wlf:layout-internal (around disable-ww-mode activate)
               (ad-deactivate-regexp "widen-window")
               (unwind-protect
                   ad-do-it
                 (ad-activate-regexp "widen-window")))

             (defadvice widen-current-window (around e2wm:disable-ww-mode activate)
               (unless (e2wm:managed-p)
                 ad-do-it
                 )))

           (defun e2wm:fix-widen-window-post-stop ()
             (ad-deactivate-regexp "e2wm:disable-ww-mode"))

           (defun e2wm:fix-widen-window ()
             (interactive)
             (when (featurep 'widen-window)
               (add-hook 'e2wm:pre-start-hook 'e2wm:fix-widen-window-pre-start)
               (add-hook 'e2wm:post-stop-hook 'e2wm:fix-widen-window-post-stop))
             )

           (e2wm:fix-widen-window)))
      ))
  )
