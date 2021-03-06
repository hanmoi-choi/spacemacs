;;; Turn on syntax highlighting for all buffers:
(global-font-lock-mode t)
(setq read-file-name-completion-ignore-case t)

(setq-default fill-column 100)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; add no new lines when "arrow-down key" at the end of a buffer
(setq next-line-add-newlines nil)

;; remove trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set text-mode as the default major mode, instead of fundamental-mode
;; The first of the two lines in parentheses tells Emacs to turn on Text mode
;; when you find a file, unless that file should go into some other mode, such
;; as C mode.
(setq-default major-mode 'org-mode)

;; delete the selection with a keypress
(delete-selection-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(setq tab-width 2)
(setq-default js2-basic-offset 4)
(setq-default elisp-basic-offset 2)
(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)
;; (flyspell-mode t)
(spacemacs|diminish flyspell-mode " ✎")
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'textmate-links)
(defun private/save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'evil-insert-state-exit-hook 'private/save-all)

;;Dired
;; I want this for dired-jump
(require 'dired-x)
(require 'dired-details)
(dired-details-install)
;; Nice listing
(setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))

;; Always copy/delete recursively
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Hide some files
(setq dired-omit-files "^\\..*$\\|^\\.\\.$")
(setq dired-omit-mode t)

;; List directories first
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'sof/dired-sort)

;; Automatically create missing directories when creating new files
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; Use ls from emacs
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; Changing the way M-< and M-> work in dired
;; Instead of taking me to the very beginning or very end, they now take me to the first or last file.
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 2))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))
(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

;; automagically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . log-view-mode))
(add-to-list 'auto-mode-alist '("\\.log\\.[0-9]*\\'" . log-view-mode))
(defun log-view-handler ()
  (end-of-buffer)
  (make-variable-buffer-local 'auto-revert-interval)
  (setq auto-revert-interval 1)
  (auto-revert-set-timer)
  (make-variable-buffer-local 'auto-revert-verbose)
  (setq auto-revert-verbose nil)
  (read-only-mode t)
  (font-lock-mode 0)
  (when (fboundp 'show-smartparens-mode)
    (show-smartparens-mode 0)))

(add-hook 'log-view-mode-hook 'log-view-handler)
(setq x-select-enable-clipboard t)
