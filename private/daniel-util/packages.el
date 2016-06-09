(setq daniel-util-packages
      '(
        vlf
        find-file-in-project
        tabbar-ruler
        multiple-cursors
        emr
        cliphist
        pdf-tools
        sx
        docker
        helm-chrome
        edbi
        restclient
        powershell
        ;; esqlite
        ;; pscv
        helm-ls-git))

(setq daniel-util-excluded-packages '())

(defun daniel-util/init-js2-refactor ()
  (use-package js2-refactor
    :init
    (progn
      (add-hook 'js2-mode-hook #'js2-refactor-mode)
      )))

(defun daniel-util/init-powershell ()
  (use-package powershell
    :mode (("\\.\\(ps\\|ps1\\)\\'" . powershell-mode))
    ))

(defun daniel-util/init-docker ()
  (use-package docker
    :init
    (progn
      (setenv "DOCKER_HOST" "tcp://192.168.99.100:2376")
      (setenv "DOCKER_CERT_PATH" "/Users/daniel.choi/.docker/machine/machines/default")
      (setenv "DOCKER_MACHINE_NAME" "default")
      )
    :config
    (progn
      (add-to-list 'evil-emacs-state-modes 'docker-containers-mode)
      (add-to-list 'evil-emacs-state-modes 'docker-images-mode)
      )
    ))

(defun daniel-util/init-pscv ()
  (use-package pscv
    ))

(defun daniel-util/init-helm-chrome ()
  (use-package helm-chrome
    ))

(defun daniel-util/init-edbi ()
  (use-package edbi
    ))

(defun daniel-util/init-sx ()
  (use-package sx
    :config
    (bind-keys :prefix "C-c s"
               :prefix-map my-sx-map
               :prefix-docstring "Global keymap for SX."
               ("q" . sx-tab-all-questions)
               ("i" . sx-inbox)
               ("o" . sx-open-link)
               ("u" . sx-tab-unanswered-my-tags)
               ("a" . sx-ask)
               ("s" . sx-search))))
(defun daniel-util/init-pdf-tools ()
  (use-package pdf-tools
    :init
    (progn
      )))

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
      (global-set-key [C-s-down] 'mc/mark-next-like-this)
      (global-set-key [C-s-up] 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-s-g") 'mc/delete-region-overlay)
      (global-set-key (kbd "<C-s-268632071>") 'mc/delete-region-overlay)
      )))

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

(defun daniel-util/init-vimish-fold ()
  (use-package vimish-fold
    ))

(defun daniel-util/init-find-file-in-project ()
  "Initialize my package"
  (use-package find-file-in-project
    ))

(defun daniel-util/init-tabbar-ruler ()
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
                       (not (string= "*terminal" (substring bn 0 9)))
                       (not (string= "*magit:" (substring bn 0 7)))
                       (not (string= "*rails*" (substring bn 0 7)))
                       (not (string= "*prodigy" (substring bn 0 8)))
                       ))))
                 (buffer-list))))
        (tabbar-mode))
      (setq tabbar-ruler-global-tabbar t)

      ;; ;; Set nil
      (define-key tabbar-mode-map (kbd "C-s-h") 'tabbar-backward-tab)
      (define-key tabbar-mode-map (kbd "<C-s-268632072>") 'tabbar-backward-tab)

      (define-key tabbar-mode-map (kbd "C-s-l") 'tabbar-forward-tab)
      (define-key tabbar-mode-map (kbd "<C-s-268632076>") 'tabbar-forward-tab)

      (define-key tabbar-mode-map (kbd "C-s-k") 'tabbar-backward-group)
      (define-key tabbar-mode-map (kbd "<C-s-268632075>") 'tabbar-backward-group)

      (define-key tabbar-mode-map (kbd "C-s-j") 'tabbar-forward-group)
      (define-key tabbar-mode-map (kbd "<C-s-268632074>") 'tabbar-forward-group)

      (tabbar-ruler-group-by-projectile-project)
      ;; (mode-icons-mode)
      )))

(defun daniel-util/init-helm-ls-git ()
  "Initialize my package"
  (use-package helm-ls-git
    :defer t
    :bind ("C-x C-d" . helm-browse-project)))

(defun daniel-util/init-restclient ()
  (use-package restclient
    :mode ("\\(\\.http\\|\\.http\\.gpg\\)\\'" . restclient-mode)
    :defer t
    :init
    (progn
      (defun restclient-http-send-current-raw-stay-in-window ()
        (interactive)
        (restclient-http-send-current t t))

      (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
        "s" 'restclient-http-send-current-stay-in-window
        "S" 'restclient-http-send-current
        "r" 'restclient-http-send-current-raw-stay-in-window
        "R" 'restclient-http-send-current-raw
        ))
    :config
    (progn
      )
    )
  )
