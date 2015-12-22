(evil-leader/set-key
  "bl" 'helm-buffers-list)

(global-set-key (kbd "<M-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

(global-set-key (kbd "M-k") 'er/expand-region)
(global-set-key (kbd "M-j") 'er/contract-region)

(global-set-key (kbd "C-c t") 'toggle-camelcase-underscores)
(global-set-key (kbd "M-d") 'private/duplicate-current-line-or-region)

(global-set-key [C-up] 'private/move-text-up)
(global-set-key [C-down] 'private/move-text-down)

(define-key global-map (kbd "C-c C-l" ) 'helm-buffers-list)
(define-key global-map (kbd "C-c C-n") 'private/indent-buffer)
(define-key global-map (kbd "C-c C-r") 'revert-buffer)

(global-set-key (kbd "C-c C-v") 'helm-show-kill-ring)

(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c D") 'dash-at-point-with-docset)

(global-set-key (kbd "C-x C-s") 'private/save-all)

(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "M-w") 'kill-this-buffer)

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(global-set-key (kbd "C-c f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c b") 'helm-projectile-switch-to-buffer)
(global-set-key (kbd "M-e") 'helm-projectile-recentf)
(global-set-key (kbd "C-M-e") 'helm-recentf)

;; Search
(global-set-key (kbd "M-f") 'helm-swoop)
(global-set-key (kbd "M-r") 'spacemacs/helm-swoop-region-or-symbol)

;;;; Opened buffers socpe
(global-set-key (kbd "C-c o") 'spacemacs/helm-buffers-smart-do-search)
(global-set-key (kbd "C-c O") 'spacemacs/helm-buffers-smart-do-search-region-or-symbol)

;;;; Project scope
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

;; Helm
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h f")	'helm-find-files)
(global-set-key (kbd "C-c h m")	'helm-man-woman)
(global-set-key (kbd "C-c h /") 'helm-find)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h a") 'helm-apropos)
(global-set-key (kbd "C-c h h g") 'helm-info-gnus)
(global-set-key (kbd "C-c h h i") 'helm-info-at-point)
(global-set-key (kbd "C-c h h r") 'helm-info-emacs)
(global-set-key (kbd "C-c h <tab>") 'helm-lisp-completion-at-point)
(global-set-key (kbd "C-c h b") 'helm-resume)
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h r") 'helm-regexp)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h t") 'helm-top)
(global-set-key (kbd "C-c h s") 'helm-surfraw)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h e") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-c h c") 'helm-calcul-expression)
(global-set-key (kbd "C-c C-l") 'helm-eshell-history)
(global-set-key (kbd "C-c C-l") 'helm-comint-input-ring)
(global-set-key (kbd "C-c C-h") 'helm-minibuffer-history)

(evil-leader/set-key
  "hs" 'helm-semantic-or-imenu
  "hf" 'helm-find-files
  "hb" 'helm-resume
  "hl" 'helm-resume
  "h/" 'helm-find
  "ho" 'helm-occur
  "hb" 'helm-buffers-list
  "ha" 'helm-all-mark-rings

  "hA" 'spacemacs/helm-files-do-ag
  "hB" 'spacemacs/helm-buffers-do-ag
  "hr" 'helm-regexp
  "hx" 'helm-register
  "hg" 'helm-google-suggest
  "he" 'helm-eval-expression-with-eldoc
  "hc" 'helm-calcul-expression
  "hm" 'helm-mini)

(require 'calendar)
(evil-set-initial-state 'calendar-mode 'emacs)

(global-set-key (kbd "C-c <f2>") 'show-file-name)

;;smartparens
(eval-after-load 'smartparens
  '(progn

     (spacemacs|define-micro-state smartparens
       :doc "[a]^ [e]$ [u]up [d]down [h](<- [j](-> [k])-> [l])<- [f]fwd [b]bwd
              [k]kill sexp [K]kill word
              [u]unwrap [s]splice [S]split
              [q]Quit"
       :persistent t
       :bindings
       ("a" sp-beginning-of-sexp)
       ("e" sp-end-of-sexp)
       ("u" sp-up-sexp)
       ("d" sp-down-sexp)
       ("h" sp-backward-slurp-sexp)
       ("j" sp-backward-barf-sexp)
       ("k" sp-forward-barf-sexp)
       ("l" sp-forward-slurp-sexp)
       ("b" sp-backward-sexp)
       ("u" sp-unwrap-sexp)
       ("s" sp-splice-sexp)
       ("S" sp-split-sexp)
       ("q" nil :exit t)
       )

     (define-key smartparens-mode-map (kbd "C-x p") 'spacemacs/smartparens-micro-state)

     (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
     (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-splice-sexp)
     (define-key smartparens-mode-map (kbd "C-M-d") 'sp-splice-sexp)

     (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
     (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
     (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
     (define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)
     (define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
     (define-key smartparens-mode-map (kbd "C-}") 'sp-select-next-thing)
     (define-key smartparens-mode-map (kbd "C-M-n") 'sp-select-next-thing)
     (define-key smartparens-mode-map (kbd "C-{") 'sp-select-previous-thing-exchange)
     (define-key smartparens-mode-map (kbd "C-M-p") 'sp-select-previous-thing-exchange)

     ;;;;;;;;;;;;;;;;;;
     ;; pair management
     (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
     (bind-key "M-(" 'lisp-state-wrap minibuffer-local-map)
     ;;; markdown-mode
     (sp-with-modes '(markdown-mode gfm-mode rst-mode)
       (sp-local-pair "*" "*"
                      :wrap "M-*"
                      :unless '(sp-point-after-word-p sp-point-at-bol-p)
                      :post-handlers '(("[d1]" "SPC"))
                      :skip-match 'sp--gfm-skip-asterisk)
       (sp-local-pair "**" "**")
       (sp-local-pair "_" "_" :wrap "M-_" :unless '(sp-point-after-word-p)))
     (defun sp--gfm-skip-asterisk (ms mb me)
       (save-excursion
         (goto-char mb)
         (save-match-data (looking-at "^\\* "))))
     ;;; lisp modes
     (sp-with-modes sp--lisp-modes
       (sp-local-pair "(" nil
                      :wrap "M-("
                      :pre-handlers '(my-add-space-before-sexp-insertion)
                      :post-handlers '(my-add-space-after-sexp-insertion)))
     (defun my-add-space-after-sexp-insertion (id action _context)
       (when (eq action 'insert)
         (save-excursion
           (forward-char (sp-get-pair id :cl-l))
           (when (or (eq (char-syntax (following-char)) ?w)
                     (looking-at (sp--get-opening-regexp)))
             (insert " ")))))

     (defun my-add-space-before-sexp-insertion (id action _context)
       (when (eq action 'insert)
         (save-excursion
           (backward-char (length id))
           (when (or (eq (char-syntax (preceding-char)) ?w)
                     (and (looking-back (sp--get-closing-regexp))
                          (not (eq (char-syntax (preceding-char)) ?'))))
             (insert " ")))))


     ;;; org-mode
     (eval-after-load 'org
       '(progn
          (sp-with-modes 'org-mode
            (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
            (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
            (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
            (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
            (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
            (sp-local-pair "«" "»"))
          (defun sp--org-skip-asterisk (ms mb me)
            (or (and (= (line-beginning-position) mb)
                     (eq 32 (char-after (1+ mb))))
                (and (= (1+ (line-beginning-position)) me)
                     (eq 32 (char-after me)))))

          ))
     ))
