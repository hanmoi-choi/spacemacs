(setq daniel-util-packages
      '(
        vlf
        ;; emamux
        find-file-in-project
        tabbar-ruler
        multiple-cursors
        ;; vimish-fold
        emr
        cliphist
        ;; airline-themes
        ;; js2-refactor

        helm-ls-git))

(setq daniel-util-excluded-packages '())

(defun daniel-util/init-js2-refactor ()
  (use-package js2-refactor
    :init
    (progn
      (add-hook 'js2-mode-hook #'js2-refactor-mode)
      )))

(defun daniel-util/init-airline-themes ()
  (use-package airline-themes
    :config
    (progn
      (setq airline-utf-glyph-separator-left      #xe0b0
            airline-utf-glyph-separator-right     #xe0b2
            airline-utf-glyph-subseparator-left   #xe0b1
            airline-utf-glyph-subseparator-right  #xe0b3
            airline-utf-glyph-branch              #xe0a0
            airline-utf-glyph-readonly            #xe0a2
            airline-utf-glyph-linenumber          #xe0a1)
      (airline-themes-set-modeline))))

(defun daniel-util/init-emr ()
  (use-package emr
    :init
    (progn
      (add-hook 'prog-mode-hook 'emr-initialize)
      (global-set-key [C-return] 'emr-show-refactor-menu))))

(defun daniel-util/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (global-set-key [C-S-down] 'mc/mark-next-like-this)
      (global-set-key [C-S-up] 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-M-g") 'mc/delete-region-overlay))))

(defun daniel-util/init-cliphist()
  (use-package cliphist
    :init
    (progn
      (evil-leader/set-key
        "Cp" 'cliphist-paste-item)
      (setq cliphist-select-item-callback
            (lambda (str) (cliphist-paste-item str))))))

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
       '(emamux:use-nearest-pane t)))))

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
                       (not (string= "*rails*" (substring bn 0 7)))
                       (not (string= "*prodigy" (substring bn 0 8)))
                       ))))
                 (buffer-list))))
        (tabbar-mode ))

      (setq tabbar-ruler-global-tabbar t)

      ;; ;; Set nil
      (define-key tabbar-mode-map [C-M-left] 'tabbar-backward-tab)
      (define-key tabbar-mode-map [C-M-right] 'tabbar-forward-tab)
      (define-key tabbar-mode-map [C-M-down] 'tabbar-backward-group)
      (define-key tabbar-mode-map [C-M-up] 'tabbar-forward-group)

      (tabbar-ruler-group-by-projectile-project)
      (add-to-list 'mode-icons '("EnhRuby" "ruby" xpm))
      (add-to-list 'mode-icons '("Javascript-IDE" "js" xpm))
      (mode-icons-mode)
      )))

(defun daniel-util/init-helm-ls-git ()
  "Initialize my package"
  (use-package helm-ls-git
    :defer t
    :bind ("C-x C-d" . helm-browse-project)))
