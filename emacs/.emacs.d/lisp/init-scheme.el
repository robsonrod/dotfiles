(use-package 
  geiser-guile
  :ensure t 
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (setq geiser-guile-binary "/usr/bin/guile" gbeiser-active-implementations '(guile))
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode))

(provide 'init-scheme)
