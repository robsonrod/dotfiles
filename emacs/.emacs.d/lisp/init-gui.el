;; icons
(use-package 
  all-the-icons 
  :if (display-graphic-p))

;; modeline icons
(use-package 
  minions 
  :hook (doom-modeline-mode . minions-mode))

;; doom themes
(use-package 
  doom-themes 
  :ensure t 
  :config (setq doom-themes-enable-bold t doom-themes-enable-italic t) 
  ;; (load-theme 'doom-dracula t) 
  (doom-themes-org-config) 
  (doom-themes-neotree-config))

;; spacemacs themes
(use-package spacemacs-theme
  :ensure t)

(use-package catppuccin-theme
 :ensure t
 :config
 (setq catppuccin-flavor 'latte))

;; doom modeline
(use-package 
  doom-modeline 
  :ensure t 
  :init (doom-modeline-mode 1) 
  :custom ((doom-modeline-height 15) 
           (doom-modeline-bar-width 6) 
           (doom-modeline-lsp t) 
           (doom-modeline-persp-name nil) 
           (doom-modeline-irc nil) 
           (doom-modeline-mu4e nil) 
           (doom-modeline-minor-modes t) 
           (doom-modeline-buffer-file-name-style 'truncate-except-project) 
           (doom-modeline-major-mode-icon t)))

(load-theme 'doom-dracula t) 

(provide 'init-gui)
