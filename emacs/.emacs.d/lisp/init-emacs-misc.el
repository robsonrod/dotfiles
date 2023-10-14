(use-package
  auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; hide minor modes
(use-package 
  diminish)

(use-package 
  perspective 
  :demand t 
  :bind (("C-M-k" . persp-switch) 
         ("C-M-n" . persp-next) 
         ("C-x k" . persp-kill-buffer*)) 
  :custom (persp-initial-frame-name "Main") 
  (persp-mode-prefix-key (kbd "C-c M-p")) 
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t) 
    (persp-mode)))

;; helper
(use-package 
  which-key 
  :init (which-key-mode) 
  :diminish which-key-mode 
  :config (setq which-key-idle-delay 0.3))

;; emacs help improvements
(use-package 
  helpful 
  :custom (counsel-describe-function-function #'helpful-callable) 
  (counsel-describe-variable-function #'helpful-variable) 
  :bind ([remap describe-function] . counsel-describe-function) 
  ([remap describe-command] . counsel-help-command) 
  ([remap describe-variable] . counsel-describe-variable) 
  ([remap describe-key] . helpful-key))

(use-package 
  ripgrep 
  :ensure t)

(use-package 
  exec-path-from-shell 
  :if (memq window-system '(mac ns x)) 
  :config (exec-path-from-shell-initialize))

(use-package
  auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-interval 2)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(robsonrod/ctrl-c-definer
  "2" '(robsonrod/split-window-two :which-key "split into two windows")
  "k" '(robsonrod/kill-current-buffer :which-key "kill current buffer")
  "K" '(robsonrod/kill-all-buffers :which-key "Kill all buffers")
  "i" '(robsonrod/open-my-config :which-key "open emacs init file")
  "e" '(eval-buffer :which-key "eval current buffer")
  "=" '(robsonrod/text-scale-restore :which-key "restore font size")
  "+" '(text-scale-increase :which-key "increase font size")
  "-" '(text-scale-decrease :which-key "decrease font size")
  )

(robsonrod/major-mode-leader-map
  "d" '(robsonrod/duplicate-line :which-key "duplicate lines")
  "u" '(robsonrod/move-line-up :which-key "move line up")
  "w" '(robsonrod/move-line-down :which-key "move line down"))

(robsonrod/ctrl-c-definer
  "d" '(robsonrod/load-dracula :which-key "dracula theme")
  "a" '(robsonrod/load-darkmode :which-key "spacemacs dark theme")
  "l" '(robsonrod/load-lightmode :which-key "spacemacs light theme"))

(provide 'init-emacs-misc)
