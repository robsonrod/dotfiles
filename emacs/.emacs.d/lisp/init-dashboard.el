(use-package 
  dashboard 
  :ensure t
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Welcome" 
        dashboard-set-init-info nil 
        show-week-agenda-p t) 
    (setq dashboard-set-heading-icons t 
        dashboard-set-file-icons t 
        dashboard-startup-banner "/home/robson/Downloads/emacs.png" 
        dashboard-footer-messages '("Happy codding")) 
  (dashboard-setup-startup-hook))

(define-key dashboard-mode-map (kbd "C-c d") #'(lambda () 
                                                 (interactive) 
                                                 (dashboard-refresh-buffer) 
                                                 (message "refreshing... done")))

(provide 'init-dashboard)
