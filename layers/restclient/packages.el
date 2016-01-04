(setq restclient-packages
      '(
        restclient
        company-restclient
        )
      )

(defun restclient/init-company-restclient ()
  (use-package company-restclient
    :init
    (eval-after-load 'company
      '(progn
         (when (configuration-layer/layer-usedp 'auto-completion)
           (add-to-list 'company-backends 'company-restclient))
         ))))

(defun restclient/init-restclient ()
  (use-package restclient
    :mode ("\\.http\\'" . restclient-mode)
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
      (defun my/load-restclient-file ()
        (interactive)
        (if (file-exists-p "~/Dropbox/restclient.http")
           (find-file "~/Dropbox/restclient.http")))
      (global-set-key (kbd "C-c <f1>") 'my/load-restclient-file)
      )
    )
  )
