(setq daniel-ruby-packages
      '(
        ;; bundler
        evil-matchit
        flycheck
        company
        ;; rbenv
        chruby
        rspec-mode
        enh-ruby-mode
        robe
        ruby-tools
        projectile-rails
        yaml-mode
        rubocop
        yard-mode
        ruby-refactor
        ruby-hash-syntax))

(defun daniel-ruby/init-ruby-hash-syntax ()
  (use-package ruby-hash-syntax
    :diminish ""
    :init
    (progn
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mx{" 'ruby-toggle-hash-syntax)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mxb" 'ruby-toggle-block))))

(defun daniel-ruby/init-chruby ()
  (use-package chruby
    ))

(defun daniel-ruby/init-ruby-refactor ()
  (use-package ruby-refactor
    :diminish ""
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch))))

(defun daniel-ruby/init-rubocop ()
  (use-package rubocop
    :diminish ""
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'rubocop-mode))))

(defun daniel-ruby/init-yard-mode ()
  (use-package yard-mode
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'yard-mode)
      (spacemacs|diminish yard-mode ""))))

(defun daniel-ruby/init-rbenv ()
  "Initialize RBENV mode"
  (use-package rbenv
    :defer t
    :init (global-rbenv-mode)
    :config (add-hook 'enh-ruby-mode-hook
                      (lambda () (rbenv-use-corresponding)))))

(defun daniel-ruby/init-rspec-mode ()
  (use-package rspec-mode
    :init
    (progn
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtT" 'rspec-find-spec-or-target-other-window)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtt" 'rspec-toggle-spec-and-target)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mta" 'rspec-verify-all)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtf" 'rspec-run-last-failed)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtv" 'rspec-verify)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtr" 'rspec-rerun)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtS" 'rspec-run-single-file)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mts" 'rspec-verify-single)
      (evil-ex-define-cmd "T" 'rspec-toggle-spec-and-target)
      )
    :config
    (progn
      (spacemacs|diminish rspec-mode "")
      ;; From Sacha
      (defun sacha/rspec-verify-single ()
        "Runs the specified example at the point of the current buffer."
        (interactive)
        (rspec-run-single-file
         (concat
          (rspec-spec-file-for (buffer-file-name))
          ":"
          (save-restriction
            (widen)
            (number-to-string (line-number-at-pos))))
         (rspec-core-options)))

      (setq compilation-scroll-output t)
      (defadvice rspec-compile (around rspec-compile-around)
        "Use BASH shell for running the specs because of ZSH issues."
        (let ((shell-file-name "/bin/bash"))
          ad-do-it))
      (ad-activate 'rspec-compile)
      (fset 'rspec-verify-single 'sacha/rspec-verify-single))))

(defun daniel-ruby/init-enh-ruby-mode ()
  "Initialize Ruby Mode"
  (use-package enh-ruby-mode
    :mode (("\\^\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|\\Env\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
    :init (setq enh-ruby-mode-map (make-sparse-keymap))
    :config
    (progn
      (setq enh-ruby-deep-indent-paren nil
            enh-ruby-hanging-paren-deep-indent-level 2)
      (add-hook 'after-init-hook 'inf-ruby-switch-setup)

      (eval-after-load "hideshow"
        '(add-to-list 'hs-special-modes-alist
                      '(enh-ruby-mode
                        "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                        (lambda (arg) (ruby-end-of-block)) nil)))
      (add-to-list 'load-path "~/.emacs.d/private/reek-emacs")
      (require 'reek)
      (add-hook 'enh-ruby-mode-hook #'reek-mode)
      (require 'rubocop)
      (add-hook 'enh-ruby-mode-hook
                (lambda ()
                  (hs-minor-mode 1) ;; Enables folding
                  (modify-syntax-entry ?: ".")
                  (modify-syntax-entry ?_ "w"))) ;; Adds ":" to the word definition
      )))

(defun daniel-ruby/init-bundler ()
  (use-package bundler
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbc" 'bundle-check)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbi" 'bundle-install)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbs" 'bundle-console)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbu" 'bundle-update)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbx" 'bundle-exec))))

(defun daniel-ruby/init-projectile-rails ()
  (use-package projectile-rails
    :defer t
    :init
    (progn
      (add-hook 'projectile-mode-hook 'projectile-rails-on))
    :config
    (progn
      (setq projectile-rails-expand-snippet nil)
      (setq projectile-rails-server-mode-ansi-colors nil)
      (spacemacs|diminish projectile-rails-mode " ⇋" " RoR")
      ;; Start projectile-rails
      (require 'enh-ruby-mode)
      (define-key enh-ruby-mode-map (kbd "C-c C-g") 'projectile-rails-goto-file-at-point)

      ;; Find files
      (evil-leader/set-key-for-mode 'enh-ruby-mode
        ;; Find files
        "mrfa" 'projectile-rails-find-locale
        "mrfc" 'projectile-rails-find-controller
        "mrfe" 'projectile-rails-find-environment
        "mrff" 'projectile-rails-find-feature
        "mrfh" 'projectile-rails-find-helper
        "mrfi" 'projectile-rails-find-initializer
        "mrfj" 'projectile-rails-find-javascript
        "mrfl" 'projectile-rails-find-lib
        "mrfm" 'projectile-rails-find-model
        "mrfn" 'projectile-rails-find-migration
        "mrfo" 'projectile-rails-find-log
        "mrfs" 'projectile-rails-find-spec
        "mrfr" 'projectile-rails-find-rake-task
        "mrfS" 'projectile-rails-find-stylesheet
        "mrfu" 'projectile-rails-find-fixture
        "mrfv" 'projectile-rails-find-view
        "mrfy" 'projectile-rails-find-layout
        "mrf@" 'projectfle-rails-find-mailer

        ;; Goto file
        "mrgc" 'projectile-rails-find-current-controller
        "mrgd" 'projectile-rails-goto-schema
        "mrge" 'projectile-rails-goto-seeds
        "mrgh" 'projectile-rails-find-current-helper
        "mrgj" 'projectile-rails-find-current-javascript
        "mrgg" 'projectile-rails-goto-gemfile
        "mrgm" 'projectile-rails-find-current-model
        "mrgn" 'projectile-rails-find-current-migration
        "mrgs" 'projectile-rails-find-current-spec
        "mrgr" 'projectile-rails-goto-routes
        "mrgS" 'projectile-rails-find-current-stylesheet
        "mrgt" 'projectile-rails-find-current-test
        "mrgu" 'projectile-rails-find-current-fixture
        "mrgv" 'projectile-rails-find-current-view
        "mrgz" 'projectile-rails-goto-spec-helper
        "mrg." 'projectile-rails-goto-file-at-point
        ;; Rails external commands
        "mrG" 'projectile-rails-generate
        "mrc" 'projectile-rails-console
        "mrr:" 'projectile-rails-rake
        "mrS" 'projectile-rails-server
        ;; Refactoring 'projectile-rails-mode
        "mrRx" 'projectile-rails-extract-region)
      )))


(defun daniel-ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init
    (progn
      (setq robe-mode-map (make-sparse-keymap))
      (add-hook 'enh-ruby-mode-hook 'robe-mode)
      (when (configuration-layer/layer-usedp 'auto-completion)
        (push 'company-robe company-backends)))
    :config
    (progn

      (spacemacs|hide-lighter robe-mode)
      ;; robe mode specific
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mgg" 'robe-jump)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mhd" 'robe-doc)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrsr" 'robe-rails-refresh)
      ;; inf-enh-ruby-mode
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msf" 'ruby-send-definition)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msF" 'ruby-send-definition-and-go)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msi" 'robe-start)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msr" 'ruby-send-region)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msR" 'ruby-send-region-and-go)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mss" 'ruby-switch-to-inf))))

(defun daniel-ruby/init-yaml-mode ()
  "Initialize YAML mode"
  (use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode))
    :config (add-hook 'yaml-mode-hook
                      '(lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

(defun daniel-ruby/init-feature-mode ()
  "Initialize Cucumber feature mode"
  (use-package feature-mode
    :mode (("\\.feature\\'" . feature-mode))))

(defun daniel-ruby/init-haml-mode ()
  (use-package haml-mode
    :defer t))

(defun daniel-ruby/post-init-evil-matchit ()
  (add-hook `enh-ruby-mode-hook `turn-on-evil-matchit-mode))

(defun daniel-ruby/post-init-flycheck ()
  (add-hook 'enh-ruby-mode-hook 'flycheck-mode))

(defun daniel-ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init
    (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
    :config
    (progn
      (spacemacs|hide-lighter ruby-tools-mode)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mx\'" 'ruby-tools-to-single-quote-string)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mx\"" 'ruby-tools-to-double-quote-string)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mx:" 'ruby-tools-to-symbol))))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode
    :defer t
    :init (add-hook 'enh-ruby-mode-hook 'ruby-test-mode)
    :config
    (progn
      (spacemacs|hide-lighter ruby-test-mode)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtb" 'ruby-test-run)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtt" 'ruby-test-run-at-point))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun ruby/post-init-company ()
    (spacemacs|add-company-hook enh-ruby-mode)
    (eval-after-load 'company-dabbrev-code
      '(push 'enh-ruby-mode company-dabbrev-code-modes))))
