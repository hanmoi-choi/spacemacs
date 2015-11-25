;;; packages.el --- daniel-db Layer packages File for Spacemacs
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
(setq daniel-db-packages
    '(
      edbi
      company-edbi))

;; List of packages to exclude.
(setq daniel-db-excluded-packages '())

(defun daniel-db/init-edbi ()
  (use-package edbi))

(defun daniel-db/init-company-edbi ()
  (use-package company-edbi
    :init (push 'company-edbi 'edbi:sql-mode)))
