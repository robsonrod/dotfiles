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
 (setq catppuccin-flavor 'macchiato))

;; doom modeline
(use-package 
  doom-modeline 
  :ensure t 
  :init (doom-modeline-mode 1) 
  :custom ((setq doom-modeline-height 15) 
           (setq doom-modeline-bar-width 6) 
           (setq doom-modeline-lsp t) 
           (setq doom-modeline-persp-name nil)
           (setq doom-modeline-battery t)
           (setq doom-modeline-irc nil)
           (setq doom-modeline-icon t)
           (setq doom-modeline-modal t)
           (setq doom-modeline-modal-modern-icon t)
           (setq doom-modeline-modal-icon t)
           (setq doom-modeline-mu4e nil) 
           (setq doom-modeline-minor-modes t) 
           (setq doom-modeline-buffer-file-name-style 'truncate-except-project) 
           (setq doom-modeline-major-mode-icon t)))

(load-theme 'catppuccin t) 

(provide 'init-gui)
