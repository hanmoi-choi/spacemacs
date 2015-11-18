(setq daniel-util-packages
      '(
        vlf
        emamux
        find-file-in-project
        tabbar-ruler
        multiple-cursors
        vimish-fold
        cliphist
        symon
        helm-ls-git))

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

(defun daniel-util/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (evil-leader/set-key
        "<down>" 'mc/mark-next-like-this
        "<up>" 'mc/mark-previous-like-this
        "<left>" 'mc/mark-all-like-this)
      (global-set-key (kbd "C-M-g") 'mc/delete-region-overlay))
    ))

(defun daniel-util/init-cliphist()
  (use-package cliphist
    :init
    (progn
      (evil-leader/set-key
        "Cp" 'cliphist-paste-item)
      (setq cliphist-select-item-callback
            (lambda (str) (cliphist-paste-item str)))
      )))

(defun daniel-util/init-vlf ()
  "Initialize my package"
  (use-package vlf
    ))

(defun daniel-util/init-emamux ()
  "Initialize my package"
  (use-package emamux
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "Es" 'emamux:send-command
        "Er" 'emamux:run-command
        "El" 'emamux:run-last-command
        "Ey" 'emamux:yank-from-list-buffers
        "Ec" 'emamux:close-runner-pane
        "EC" 'emamux:copy-kill-ring
        "Ei" 'emamux:interrupt-runner)
      (custom-set-variables
       '(emamux:completing-read-type 'helm)
       '(emamux:use-nearest-pane t))
      )))

(defun daniel-util/init-symon ()
  "Initialize my package"
  (use-package symon
    :init
    (progn
      (setq symon-sparkline-type 'boxed)
      (symon-mode 1))))

(defun daniel-util/init-vimish-fold ()
  (use-package vimish-fold
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
