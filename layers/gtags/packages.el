;;; packages.el --- gtags Layer packages File for Spacemacs
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

(setq gtags-packages
  '(
    helm-gtags
    ggtags
    ))

(defun gtags/init-ggtags ()
  (use-package ggtags
    :defer t))

(defun gtags/init-helm-gtags ()
  (use-package helm-gtags
    :defer t
    :diminish "[HG] "
    :init
    (progn
      (setq helm-gtags-ignore-case t
            helm-gtags-auto-update t
            helm-gtags-use-input-at-cursor t
            helm-gtags-pulse-at-cursor t)
      ;; modes that do not have a layer, define here
      (spacemacs/helm-gtags-define-keys-for-mode 'java-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'enh-ruby-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'web-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'js2-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'emacs-lisp-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'common-lisp-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'shell-script-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'awk-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'dired-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'compilation-mode)
      (spacemacs/helm-gtags-define-keys-for-mode 'shell-mode)

      (spacemacs/ggtags-enable-eldoc 'java-mode)
      (spacemacs/ggtags-enable-eldoc 'enh-ruby--mode)
      (spacemacs/ggtags-enable-eldoc 'js2-mode)
      (spacemacs/ggtags-enable-eldoc 'web-mode)
      )
    :config
    (progn
      ;; if anyone uses helm-gtags, they would want to use these key bindings
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack))))
