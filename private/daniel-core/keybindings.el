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
(global-set-key (kbd "<C-s-268632092>") 'indent-region)

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
(global-set-key (kbd "<C-s-268632069>") 'helm-recentf)

;; Search
(global-set-key (kbd "s-f") 'helm-swoop)
(global-set-key (kbd "s-r") 'spacemacs/helm-swoop-region-or-symbol)

;;;; Opened buffers socpe
(global-set-key (kbd "C-c o") 'spacemacs/helm-buffers-smart-do-search)
(global-set-key (kbd "C-c O") 'spacemacs/helm-buffers-smart-do-search-region-or-symbol)

;;;; Project scope
(global-set-key (kbd "C-s-f") 'spacemacs/helm-project-smart-do-search)
(global-set-key (kbd "<C-s-268632070>") 'spacemacs/helm-project-smart-do-search)

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
     (defmacro def-pairs (pairs)
       `(progn
          ,@(loop for (key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))

     (def-pairs ((paren        . "(")
                 (bracket      . "[")
                 (brace        . "{")
                 (single-quote . "'")
                 (double-quote . "\"")
                 (back-quote   . "`")))

     (bind-keys
      :map smartparens-mode-map
      ("C-M-a" . sp-beginning-of-sexp)
      ("C-M-e" . sp-end-of-sexp)

      ("M-s-<down>" . sp-down-sexp)
      ("M-s-<up>"   . sp-up-sexp)
      ("M-<down>" . sp-backward-down-sexp)
      ("M-<up>"   . sp-backward-up-sexp)

      ("C-M-f" . sp-forward-sexp)
      ("C-M-b" . sp-backward-sexp)

      ("C-M-n" . sp-next-sexp)
      ("C-M-p" . sp-previous-sexp)

      ("C-S-f" . sp-forward-symbol)
      ("C-S-b" . sp-backward-symbol)

      ("M-s-<right>" . sp-forward-slurp-sexp)
      ("M-<right>" . sp-forward-barf-sexp)
      ("M-s-<left>"  . sp-backward-slurp-sexp)
      ("M-<left>"  . sp-backward-barf-sexp)

      ("C-M-t" . sp-transpose-sexp)
      ("C-M-k" . sp-kill-sexp)
      ("C-M-w" . sp-copy-sexp)

      ("M-<delete>" . sp-unwrap-sexp)
      ("M-<backspace>" . sp-splice-sexp)

      ("C-<backspace>" . sp-backward-kill-word)

      ("M-[" . sp-backward-unwrap-sexp)
      ("M-]" . sp-unwrap-sexp)

      ("C-x C-t" . sp-transpose-hybrid-sexp)

      ("C-c ("  . wrap-with-parens)
      ("C-c ["  . wrap-with-brackets)
      ("C-c {"  . wrap-with-braces)
      ("C-c '"  . wrap-with-single-quotes)
      ("C-c \"" . wrap-with-double-quotes)
      ("C-c _"  . wrap-with-underscores)
      ("C-c `"  . wrap-with-back-quotes))
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
