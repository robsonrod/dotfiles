(use-package 
  rust-mode 
  :defer t 
  :mode "\\.rs\\'" 
  :custom (rust-format-on-save t) 
  (lsp-rust-server 'rust-analyzer))

(provide 'init-rust)
