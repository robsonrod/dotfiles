;; treemac
(use-package 
  treemacs 
  :ensure t 
  :bind (:map global-map
              ("M-0" . treemacs-select-window) 
              ("C-x t 1" . treemacs-no-delete-other-windows) 
              ("C-x t t" . treemacs) 
              ("C-x t d" . treemacs-select-directory)) 
  :config (progn 
            (setq treemacs-no-png-images t treemacs-is-never-other-window nil)))

(use-package 
  treemacs-all-the-icons 
  :config (treemacs-load-theme "all-the-icons"))

(provide 'init-treemacs)
