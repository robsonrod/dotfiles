(use-package 
  geiser-mit 
  :ensure t 
  :config (setq geiser-mit-binary "/usr/bin/scheme" gbeiser-active-implementations '(mit)))

(provide 'init-scheme)
