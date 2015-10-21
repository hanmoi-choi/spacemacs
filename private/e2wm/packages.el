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
      ;; package names go here
      e2wm
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

(defun e2wm/init-e2wm ()
  :defer t
  :init
  (progn
    (setq e2wm:c-max-history-num 40)
    (setq e2wm:c-code-recipe
          '(| (:right-max-size 20)
              (- (:upper-size-ratio 0.7)
                 main sub)
              history))
    (setq e2wm:c-code-winfo
          '((:name history :plugin history-list)
            (:name sub :buffer "*helm*" :default-hide t)
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
    ;; (setq e2wm:c-code-winfo
    ;;   '((:name main)
    ;;     (:name files :plugin files)
    ;;     (:name history :plugin history-list)
    ;;     (:name sub :buffer "*info*" :default-hide t)
    ;;     (:name imenu :plugin imenu :default-hide nil))
    ;;   )
    ;; (setq e2wm:c-code-recipe
    ;;   '(| (:left-max-size 30)
    ;;       (- (:upper-size-ratio 0.7)
    ;;          files history)
    ;;       (- (:upper-size-ratio 0.7)
    ;;          (| (:right-max-size 30)
    ;;             main imenu)
    ;;          sub)))
    ;; (add-to-list 'evil-emacs-state-modes 'e2wm:)
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
    (require 'e2wm)
    (e2wm:add-keymap
     e2wm:pst-minor-mode-keymap
     '(("M-]"       . e2wm:pst-history-forward-command) ; 履歴を進む
       ("M-["       . e2wm:pst-history-back-command) ; 履歴をもどる
       ) e2wm:prefix-key)
    ))
