;; completion framework
(use-package 
  ivy 
  :ensure t 
  :diminish 
  :bind (("C-s" . swiper) :map ivy-minibuffer-map ("TAB" . ivy-alt-done) 
         ("C-f" . ivy-alt-done) 
         ("C-l" . ivy-alt-done) 
         ("C-j" . ivy-next-line) 
         ("C-k" . ivy-previous-line) 
         :map ivy-switch-buffer-map ("C-k" . ivy-previous-line) 
         ("C-l" . ivy-done) 
         ("C-d" . ivy-switch-buffer-kill) 
         :map ivy-reverse-i-search-map ("C-k" . ivy-previous-line) 
         ("C-d" . ivy-reverse-i-search-kill)) 
  :init (ivy-mode 1) 
  (setq ivy-use-virtual-buffers t) 
  (setq ivy-wrap t) 
  (setq ivy-count-format "(%d/%d) ") 
  (setq enable-recursive-minibuffers t)

  ;; use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist) 
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15) 
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15) 
  (setf (alist-get 'swiper ivy-height-alist) 15) 
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

;; enhance completion framework
(use-package 
  counsel 
  :bind (("M-x" . counsel-M-x) 
         ("C-x b" . counsel-ibuffer) 
         ("C-x C-f" . counsel-find-file) 
         ("C-c r" . counsel-rg) 
         ("C-c f" . counsel-fzf) 
         ("C-M-j" . counsel-switch-buffer) 
         ("M-y" . counsel-yank-pop) 
         :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history)) 
  :config (setq ivy-initial-inputs-alias nil))

;; friendly gui for ivy
(use-package 
  ivy-rich 
  :after ivy 
  counsel 
  :init (ivy-rich-mode 1))

(provide 'init-completion)
