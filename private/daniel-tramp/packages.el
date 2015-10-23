;;; packages.el --- daniel-tramp Layer packages File for Spacemacs
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
(setq daniel-tramp-packages
      '(
        tramp
        ))

;; List of packages to exclude.
(setq daniel-tramp-excluded-packages '())

;; For each package, define a function daniel-tramp/init-<package-name>
;;
;; (defun daniel-tramp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun daniel-tramp/init-tramp ()
  (progn
    (require 'tramp)
    (setq explicit-shell-file-name "/bin/bash")
    (setq tramp-default-method "ssh")
    (setq tramp-verbose 9)
    ;; This is important, or with fancy shell, tramp will not work
    (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
    (setq tramp-ssh-controlmaster-options
          (concat
           "-o ControlPath=~/.ssh/sockets/%%r@%%h:%%p "
           "-o ControlMaster=auto -o ControlPersist=yes"))

    (defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
      "*The filename prefix used to open a file with `find-file-root'.")

    (defvar find-file-root-history nil
      "History list for files found using `find-file-root'.")

    (defvar find-file-root-hook nil
      "Normal hook for functions to run after finding a \"root\" file.")

    (defun find-file-root ()
      "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

      (interactive)
      (require 'tramp)
      (let* ( ;; We bind the variable `file-name-history' locally so we can
             ;; use a separate history list for "root" files.
             (file-name-history find-file-root-history)
             (name (or buffer-file-name default-directory))
             (tramp (and (tramp-tramp-file-p name)
                         (tramp-dissect-file-name name)))
             path dir file)

        ;; If called from a "root" file, we need to fix up the path.
        (when tramp
          (setq path (tramp-file-name-localname tramp)
                dir (file-name-directory path)))

        (when (setq file (read-file-name "Find file (UID = 0): " dir path))
          (find-file (concat find-file-root-prefix file))
          ;; If this all succeeded save our new history list.
          (setq find-file-root-history file-name-history)
          ;; allow some user customization
          (run-hooks 'find-file-root-hook))))
    (global-set-key (kbd "C-x C-r") 'find-file-root)

    (defun my-mode-line-function ()
      (when (string-match "^/su\\(do\\)?:" default-directory)
        (setq mode-line-format
              (format-mode-line mode-line-format 'font-lock-warning-face))))))
