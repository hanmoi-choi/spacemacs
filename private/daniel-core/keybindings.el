(evil-leader/set-key
  "bl" 'helm-buffers-list)

(global-set-key (kbd "C-c C-v" ) 'preview-file-with-marked2)
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<s-f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

(global-set-key (kbd "s-k") 'er/expand-region)
(global-set-key (kbd "s-j") 'er/contract-region)
(global-set-key (kbd "M-k") 'er/expand-region)
(global-set-key (kbd "M-j") 'er/contract-region)

(global-set-key (kbd "C-x t") 'toggle-camelcase-underscores)
(global-set-key (kbd "s-d") 'private/duplicate-current-line-or-region)

(global-set-key [C-up] 'private/move-text-up)
(global-set-key [C-down] 'private/move-text-down)

(define-key global-map (kbd "C-c C-l" ) 'helm-buffers-list)
(define-key global-map (kbd "C-c C-n") 'private/indent-buffer)
(define-key global-map (kbd "C-c C-r") 'revert-buffer)
(define-key global-map (kbd "C-c r") 'revert-buffer)
(define-key global-map (kbd "C-c b") 'bury-buffer)

(global-set-key (kbd "C-s-\\") 'indent-region)

(global-set-key (kbd "s-g") 'helm-ls-git-ls)
(global-set-key (kbd "s-o") 'ibuffer)
(global-set-key (kbd "s-i") 'helm-imenu)
(global-set-key (kbd "s-m") 'woman)
(global-set-key (kbd "s-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c D") 'dash-at-point-with-docset)

(global-set-key (kbd "C-x C-s") 'private/save-all)

(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))
(global-set-key (kbd "M-.") 'helm-gtags-dwim)
(global-set-key (kbd "s-.") 'helm-gtags-dwim)
(global-set-key (kbd "s-,") 'helm-gtags-pop-stack)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(global-set-key (kbd "C-c f") 'helm-projectile-find-file)
(global-set-key (kbd "s-e") 'helm-projectile-recentf)
(global-set-key (kbd "C-s-e") 'helm-recentf)

;; Search
(global-set-key (kbd "s-f") 'helm-swoop)
(global-set-key (kbd "s-r") 'spacemacs/helm-swoop-region-or-symbol)

;;;; Opened buffers socpe
(global-set-key (kbd "C-c o") 'spacemacs/helm-buffers-smart-do-search)
(global-set-key (kbd "C-c O") 'spacemacs/helm-buffers-smart-do-search-region-or-symbol)

;;;; Project scope
(global-set-key (kbd "C-s-f") 'spacemacs/helm-project-smart-do-search)
(global-set-key (kbd "C-c C-p") 'neotree-project-dir)

(global-unset-key (kbd "M-/"))
;; Helm
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
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
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-c h e") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-c h c") 'helm-calcul-expression)
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

(global-set-key (kbd "C-c <f2>") 'show-file-name)

(evil-leader/set-key
  "Ebr" 'persp-remove-buffer
  "Eba" 'persp-add-buffer
  "Ea" 'persp-add-new
  "Ek" 'persp-kill
  "Er" 'persp-rename
  "Eh" 'spacemacs/helm-perspectives
  "Es" 'persp-save-state-to-file
  "El" 'persp-load-state-from-file)

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
       ("q" nil :exit t))

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
     ))

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; Ace Link
(eval-after-load 'ace-link
  '(progn
     (require 'woman)
     (global-set-key (kbd "s-l") 'ace-link-addr)
     (define-key woman-mode-map (kbd "s-l") 'ace-link-woman)
     (define-key org-mode-map (kbd "s-l") 'ace-link-org)
     (define-key w3m-mode-map (kbd "s-l") 'ace-link-eww)
     ))
