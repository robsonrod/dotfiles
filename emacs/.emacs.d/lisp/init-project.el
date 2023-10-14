;; project manager
(use-package 
  projectile 
  :diminish projectile-mode 
  :config (projectile-mode) 
  (setq projectile-globally-ignored-directories (append '(".git"
                                                          ".ccls_cache") projectile-globally-ignored-directories))
  :demand t 
  :custom ((projectile-completion-system 'ivy)) 
  :bind-keymap ("C-c p" . projectile-command-map) 
  :init (when (file-directory-p "~/projects/personal/") 
          (setq projectile-project-search-path '("~/projects/personal/"))) 
  (setq projectile-switch-project-action #'projectile-dired))

;; ivy integration project manager
(use-package 
  counsel-projectile 
  :after projectile 
  :bind (("C-M-p" . counsel-projectile-find-file)) 
  :config (counsel-projectile-mode))

;; find file in project
(use-package 
  find-file-in-project 
  :if (executable-find "fdfind") 
  :init (when (executable-find "fd") 
          (setq ffip-use-rust-fd t)) 
  :bind (("C-c o" . ffap) 
         ("C-c p" . ffip)))

(provide 'init-project)
