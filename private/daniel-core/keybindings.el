(evil-leader/set-key
  "wC" 'ace-delete-window
  "Nb" 'spacemacs/new-empty-buffer
  "bl" 'helm-buffers-list)

(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "M-k") 'er/expand-region)
(global-set-key (kbd "M-j") 'er/contract-region)

(global-set-key (kbd "C-c t") 'toggle-camelcase-underscores)
(global-set-key (kbd "M-d") 'private/duplicate-current-line-or-region)

(global-set-key [M-up] 'private/move-text-up)
(global-set-key [M-down] 'private/move-text-down)

(define-key global-map (kbd "C-c C-l" ) 'helm-buffers-list)
(define-key global-map "\C-c\C-n" 'private/indent-buffer)
(define-key global-map "\C-c\C-y" 'bury-buffer)
(define-key global-map "\C-c\C-r" 'revert-buffer)

(global-set-key (kbd "C-c C-v") 'helm-show-kill-ring)

(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c D") 'dash-at-point-with-docset)

(global-set-key (kbd "C-x C-s") 'private/save-all)

;; C-8 will increase opacity (== decrease transparency)
;; C-9 will decrease opacity (== increase transparency
;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "M-w") 'kill-this-buffer)
(global-set-key (kbd "C-c m") 'spacemacs/toggle-maximize-buffer)
(global-set-key (kbd "<M-backspace>") 'kill-region)

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(global-set-key (kbd "C-c f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c b") 'helm-projectile-switch-to-buffer)
(global-set-key (kbd "s-e") 'helm-projectile-recentf)
(global-set-key (kbd "M-e") 'helm-projectile-recentf)
(global-set-key (kbd "C-s-e") 'helm-recentf)
(global-set-key (kbd "C-M-e") 'helm-recentf)

;; Search
(global-set-key (kbd "s-f") 'helm-swoop)
(global-set-key (kbd "M-f") 'helm-swoop)
(global-set-key (kbd "s-r") 'spacemacs/helm-swoop-region-or-symbol)
(global-set-key (kbd "M-r") 'spacemacs/helm-swoop-region-or-symbol)

;;;; Opened buffers socpe
(global-set-key (kbd "C-c o") 'spacemacs/helm-buffers-smart-do-search)
(global-set-key (kbd "C-c O") 'spacemacs/helm-buffers-smart-do-search-region-or-symbol)

;;;; Project scope
(global-set-key (kbd "C-s-f") 'spacemacs/helm-project-smart-do-search)
(global-set-key (kbd "C-M-f") 'spacemacs/helm-project-smart-do-search)
(global-set-key (kbd "C-c C-p") 'neotree-project-dir)

(global-unset-key (kbd "M-/"))

(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'evil-yank)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "M-X") 'kill-region)
(global-set-key (kbd "M-z") 'undo-tree-undo)
(global-set-key (kbd "M-s")
                (lambda ()
                  (interactive)
                  (call-interactively (key-binding "\C-x\C-s"))))
(global-set-key (kbd "M-Z") 'undo-tree-redo)

(when (spacemacs/system-is-mac)
  ;; this is only applicable to GUI mode
  (when (display-graphic-p)
    ;; Treat command as super
    (setq mac-command-key-is-meta nil)
    (setq mac-command-modifier 'super)

    (setq mac-option-key-is-meta t)
    (setq mac-option-modifier 'meta)

    ;; Keybindings
    (global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
    (global-set-key (kbd "s--") 'spacemacs/scale-down-font)
    (global-set-key (kbd "s-0") 'spacemacs/reset-font-size)
    (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
    (global-set-key (kbd "s-v") 'yank)
    (global-set-key (kbd "s-c") 'evil-yank)
    (global-set-key (kbd "s-a") 'mark-whole-buffer)
    (global-set-key (kbd "s-x") 'kill-region)
    (global-set-key (kbd "s-w") 'delete-window)
    (global-set-key (kbd "s-W") 'delete-frame)
    (global-set-key (kbd "s-n") 'make-frame)
    (global-set-key (kbd "s-z") 'undo-tree-undo)
    (global-set-key (kbd "s-s")
                    (lambda ()
                      (interactive)
                      (call-interactively (key-binding "\C-x\C-s"))))
    (global-set-key (kbd "s-Z") 'undo-tree-redo)
    (global-set-key (kbd "C-s-f") 'spacemacs/toggle-frame-fullscreen)
    ;; Emacs sometimes registers C-s-f as this weird keycode
    (global-set-key (kbd "<C-s-268632070>") 'spacemacs/toggle-frame-fullscreen)))

(add-to-list 'load-path (car (file-expand-wildcards "~/.emacs.d/vendor/org-plus-contrib")))
(require 'org-mime)
(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))
