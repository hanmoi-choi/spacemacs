(setq daniel-ruby-packages
      '(
        ;; bundler
        company
        rvm
        rspec-mode
        enh-ruby-mode
        robe
        ruby-tools
        projectile-rails
        yaml-mode
        rubocop
        ruby-hash-syntax
        yard-mode
        ))

(defun daniel-ruby/init-rubocop ()
  (use-package rubocop
    :diminish ""
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'rubocop-mode))))

(defun daniel-ruby/init-ruby-end ()
  (use-package ruby-end
    :diminish ""
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'ruby-end-mode)
      )
    ))

(defun daniel-ruby/init-ruby-hash-syntax ()
  (use-package ruby-hash-syntax
    ))

(defun daniel-ruby/init-yard-mode ()
  (use-package yard-mode
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'yard-mode)
      (spacemacs|diminish yard-mode ""))))

(defun daniel-ruby/init-rvm ()
  "Initialize RVM mode"
  (use-package rvm
    :defer t
    :init (rvm-use-default)
    ))

(defun daniel-ruby/init-rspec-mode ()
  (use-package rspec-mode
    :config
    (progn
      (defadvice rspec-compile (around rspec-compile-around)
        "Use BASH shell for running the specs because of ZSH issues."
        (let ((shell-file-name "/bin/bash"))
          ad-do-it))
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

      (ad-activate 'rspec-compile)
      (setq compilation-scroll-output t)
      (setq rspec-command-options "--color")
      (fset 'rspec-verify-single 'sacha/rspec-verify-single))))

(defun daniel-ruby/init-enh-ruby-mode ()
  "Initialize Ruby Mode"
  (use-package enh-ruby-mode
    :mode (("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
    :init (setq enh-ruby-mode-map (make-sparse-keymap))
    :config
    (progn
      (add-hook 'after-init-hook 'inf-ruby-switch-setup)
      (add-hook 'enh-ruby-mode-hook
                (lambda ()
                  (hs-minor-mode 1) ;; Enables folding
                  (modify-syntax-entry ?: ".")
                  (modify-syntax-entry ?_ "w"))) ;; Adds ":" to the word definition

      (require 'smartparens)
      (sp-local-pair 'enh-ruby-mode "def" "end")
      (sp-local-pair 'enh-ruby-mode "do" "end")

      (setq enh-ruby-deep-indent-paren nil
            enh-ruby-hanging-paren-deep-indent-level 2)
      (add-hook 'enh-ruby-mode-hook 'company-mode)

      (setq flyspell-issue-message-flg nil)
      (add-hook 'enh-ruby-mode-hook
                (lambda () (flyspell-prog-mode)))
      )))

(defun daniel-ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init
    (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
    :config
    ))

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
      (spacemacs|diminish projectile-rails-mode " ⇋" " RoR")
      ;; Start projectile-rails
      (require 'enh-ruby-mode)
      (define-key enh-ruby-mode-map (kbd "C-c C-g") 'projectile-rails-goto-file-at-point)

      (defun projectile-rails-find-view-model ()
        (interactive)
        (projectile-rails-find-resource
         "view_model: "
         '(("app/view_models/" "/view_models/\\(.+\\)\\.rb$"))
         "app/view_models/${filename}.rb"))

      (defun projectile-rails-find-current-view-model ()
        (interactive)
        (projectile-rails-find-current-resource "app/view_models/"
                                                "app/view_models/\\(.*${plural}\\)\\.rb$"
                                                'projectile-rails-find-view-model))

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
        "mrfM" 'projectile-rails-find-view-model
        "mrfn" 'projectile-rails-find-migration
        "mrfo" 'projectile-rails-find-log
        "mrfs" 'projectile-rails-find-spec
        "mrfr" 'projectile-rails-find-rake-task
        "mrfS" 'projectile-rails-find-stylesheet
        "mrfu" 'projectile-rails-find-fixture
        "mrfv" 'projectile-rails-find-view
        "mrfV" 'projectile-rails-find-view-model
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
      ;; Ex-commands

      (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test))))

(defun daniel-ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'robe-mode)
      (setq robe-mode-map (make-sparse-keymap)))
    :config
    (progn
      (spacemacs|hide-lighter robe-mode)
      ;; robe mode specific
      (spacemacs|diminish robe-mode "")
      (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
        (rvm-activate-corresponding-ruby))
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
