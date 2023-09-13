(defun with-face (str &rest face-plist) 
  (propertize str 'face face-plist))

(defun eshell-prompt () 
  (let ((header-bg "#282a36")) 
    (concat (with-face (concat (eshell/pwd) " ") 
                       :background header-bg :foreground "#50fa7b") 
            (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) 
                       :background header-bg 
                       :foreground "#f1fa8c") 
            (with-face (or (ignore-errors (format "(%s)" (vc-responsible-backend
                                                          default-directory))) 
                           "") 
                       :background header-bg :foreground "#ff5555") 
            (with-face "\n" 
                       :background header-bg) 
            (with-face user-login-name 
                       :foreground "#ff79c6") "@" (with-face "localhost" 
                                                             :foreground "#bd93f9") 
                       (if (= (user-uid) 0) 
                           (with-face " #" 
                                      :foreground "red") " $") " ")))

(defun rar/eshell-config () 
  (use-package 
    xterm-color) 
  (add-hook 'eshell-pre-command-hook (lambda () 
                                       (setenv "TERM" "xterm-256color"))) 
  (add-hook 'eshell-post-command-hook (lambda () 
                                        (setenv "TERM" "dumb"))) 
  (setq eshell-prompt-function 'eshell-prompt eshell-highlight-prompt nil ;;
        eshell-buffer-shorthand t         ;;
        eshell-history-size 5000          ;;
        eshell-buffer-maximum-lines 12000 ;; truncate after 12k lines
        eshell-hist-ignoredups t          ;; ignore duplicates
        eshell-error-if-no-glob t         ;;
        eshell-glob-case-insensitive t    ;;
        eshell-scroll-to-bottom-on-input 'all ;;
        eshell-list-files-after-cd t          ;;
        eshell-aliases-file (concat user-emacs-directory "eshell/alias") ;;
        eshell-banner-message "" ;; welcome message
        ) 
  (setq eshell-visual-commands '("vim" "nvim" "screen" "top" "less" "more" "lynx" "ncftp" "pine"
                                 "tin" "trn" "elm" "vim" "nmtui" "alsamixer" "htop" "el" "elinks"
                                 "btn")) 
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

  (use-package eshell-git-prompt
    :ensure t
    :config
    (eshell-git-prompt-use-theme 'powerline))

  )

(defun eshell-clear-buffer () 
  "Clear terminal." 
  (interactive) 
  (let ((inhibit-read-only t)) 
    (erase-buffer) 
    (eshell-send-input)))

(use-package 
  eshell 
  :hook (eshell-first-time-mode . rar/eshell-config) 
  :init)

(setenv "EXA_COLORS"
        "uu=36:gu=37:sn=32:sb=32:da=34:ur=34:uw=35:ux=36:ue=36:gr=34:gw=35:gx=36:tr=34:tw=35:tx=36:")

(use-package 
  esh-autosuggest 
  :hook (eshell-mode . esh-autosuggest-mode) 
  :ensure t)

(use-package 
  fish-completion 
  :hook (eshell-mode . fish-completion-mode))

(use-package 
  eshell-syntax-highlighting 
  :after esh-mode 
  :config (eshell-syntax-highlighting-global-mode +1))

(use-package 
  esh-autosuggest 
  :hook (eshell-mode . esh-autosuggest-mode) 
  :config (setq esh-autosuggest-delay 0.5) 
  (set-face-foreground 'company-preview-common "#4b5668") 
  (set-face-background 'company-preview nil))

(use-package 
  eshell-toggle 
  :bind ("C-x e" . eshell-toggle) 
  :custom (eshell-toggle-size-fraction 3) 
  (eshell-toggle-use-projectile-root t) 
  (eshell-toggle-run-command nil))

(message "loading eshell config")
(provide 'init-eshell)
