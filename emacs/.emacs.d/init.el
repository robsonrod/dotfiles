;;; init.el --- my emacs file -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;; My Emacs file

;;; Code:

(message "Start to load init.el")
;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; gc
(setq gc-cons-threshold (* 50 1000 1000))

;;; profile
(add-hook 'emacs-startup-hook (lambda () 
                                (message "*** Emacs loaded in %s with %d garbage collections."
                                         (format "%.2f seconds" (float-time (time-subtract
                                                                             after-init-time
                                                                             before-init-time)))
                                         gcs-done)))

;; fix obsolete warning
(setq byte-compile-warnings '(cl-functions))

;; set variables
(setq inhibit-startup-message t tab-always-indent 'complete)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; general variables
(setq inhibit-startup-message t         ; no welcome buffer
      initial-scratch-message nil       ; no scratch buffer
      ring-bell-function 'ignore        ; never ding
      history-length 20                 ; max history saves
      use-dialog-box nil                ; no ugly dialogs
      case-fold-search nil              ; case sensitive search
      confirm-kill-processes nil        ; just quit
      global-auto-revert-non-file-buffers t ; update buffers thar are non-files too
      sentence-end-double-space nil         ; no way double spaces
      load-prefer-newer t                   ; always load the new file
      tab-always-indent 'complete       ; use TAB to complete symbols
      native-comp-async-report-warnings-erros 'silent ; there's not very much I can do
      mouse-wheel-scroll-amount '(2 ((shift) . 1))    ; scroll 2 lines
      mouse-wheel-progressive-speed nil ; don't accelerate
      mouse-wheel-follow-mouse 't   ; scroll window under mouse cursor
      scroll-step 1 explicit-shell-file-name "/usr/bin/bash" shell-file-name "bash"
      dictionary-server "dict.org")
                                        ; scroll 1 line with keyboard

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) backup-by-copying t ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old
;; window title
(setq frame-title-format "%b - emacs")

;; window resize
(set-frame-parameter (selected-frame) 'alpha '(95 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; enable/disable modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(auto-revert-mode 1)
(delete-selection-mode t)
(column-number-mode t)
(save-place-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode +1)
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook vterm-mode-hook shell-mode-hook eshell-mode-hook
                              dashboard-mode-hook)) 
  (add-hook mode (lambda () 
                   (display-line-numbers-mode -1))))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; font configuration
(set-face-attribute 'default nil 
                    :font "Fira Code Retina" 
                    :height 100)
(set-face-attribute 'fixed-pitch nil 
                    :font "JetBrainsMono Nerd Font" 
                    :height 100)
(set-face-attribute 'variable-pitch nil 
                    :font "Iosevka Nerd Font" 
                    :height 100)

;; yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; Custom functions

(defun smart-open-line-above () 
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode." 
  (interactive) 
  (move-beginning-of-line nil) 
  (newline-and-indent) 
  (forward-line -1) 
  (indent-according-to-mode))

(defun smart-open-line () 
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode." 
  (interactive) 
  (move-end-of-line nil) 
  (newline-and-indent))

(global-set-key [(control shift return)] 'smart-open-line-above)
(global-set-key [(shift return)] 'smart-open-line)

;; Open my config
(defun open-my-config () 
  "Open my config file." 
  (interactive) 
  (find-file user-init-file))
(global-set-key (kbd "C-x C-i") 'open-my-config)

;; ESC cancels all commands
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; eval buffer
(global-set-key (kbd "C-c C-e") 'eval-buffer)

;; elisp format
(global-set-key (kbd "C-c e f") 'elisp-format-buffer)

;; adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; custom window management
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

;; split window
(defun pt/split-window-two () 
  "Split a window into two." 
  (interactive) 
  (split-window-right) 
  (balance-windows))
(global-set-key (kbd "C-c 2") #'pt/split-window-two)

;; buffer killer
(defun kill-this-buffer () 
  "Kill the current buffer." 
  (interactive) 
  (kill-buffer nil))

(defun kill-all-buffers () 
  "Close all buffers." 
  (interactive) 
  (let ((lsp-restart 'ignore)) 
    (delete-other-windows) 
    (save-some-buffers) 
    (let ((kill-buffer-query-functions '())) 
      (mapc 'kill-buffer (buffer-list)))))
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-x K") #'kill-all-buffers)
(global-set-key (kbd "C-x w") #'kill-this-buffer)

;; configure package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/") 
                         ("org" . "https://orgmode.org/elpa/") 
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; install package manager
(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

;; file explorer
(use-package 
  dired 
  :ensure nil 
  :defer 1 
  :commands (dired dired-jump) 
  :bind ("C-x C-j" . dired-jump) 
  :config (autoload 'dired-omit-mode "dired-x") 
  (add-hook 'with-eval-after-load (lambda () 
                                    (interactive) 
                                    (dired-collapse))) 
  (add-hook 'dired-mode-hook (lambda () 
                               (interactive) 
                               (dired-omit-mode 1) 
                               (dired-hide-details-mode 1) 
                               (all-the-icons-dired-mode 1) 
                               (hl-line-mode 1))) 
  (use-package 
    dired-rainbow 
    :defer 2 
    :config (dired-rainbow-define-chmod directory "#6cb2eb" "d.*") 
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml"
                                          "mustache" "xhtml")) 
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn"
                                         "rss" "yaml" "yml" "rdata")) 
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf"
                                              "djvu" "epub" "odp" "ppt" "pptx")) 
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod"
                                              "rst" "tex" "textfile" "txt")) 
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc")) 
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv"
                                           "ogg" "mov" "mid" "midi" "wav" "aiff" "flac")) 
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd"
                                           "eps" "svg")) 
    (dired-rainbow-define log "#c17d11" ("log")) 
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim")) 
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql"
                                                 "sql" "r" "clj" "cljs" "scala" "js")) 
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx"
                                              "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90"
                                              "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java")) 
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi")) 
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar"
                                                "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar")) 
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf"
                                              "vpk" "bsp")) 
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12"
                                               "pem")) 
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf")) 
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk"
                                               "bak")) 
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules")) 
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(use-package 
  dired-single)

(use-package 
  dired-ranger 
  :defer t)

(use-package 
  dired-collapse 
  :defer t)

(use-package 
  all-the-icons-dired 
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package 
  dired-open 
  :config (setq dired-open-extensions '(("png" . "feh") 
                                        ("mkv" . "mpv") 
                                        ("png" . "sxiv") 
                                        ("jpg" . "sxiv") 
                                        ("gif" . "sxiv") 
                                        ("mkv" . "mpv") 
                                        ("mp4" . "mpv") 
                                        ("mp3" . "mpv") 
                                        ("ogg" . "mpv"))))

;; hide dotfiles
(use-package 
  dired-hide-dotfiles 
  :hook (dired-mode . dired-hide-dotfiles-mode) 
  :config (define-key dired-mode-map "H" 'dired-hide-dotfiles-mode))

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
  (load-theme 'doom-dracula t) 
  (doom-themes-org-config) 
  (doom-themes-neotree-config))

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

;; helper
(use-package 
  which-key 
  :init (which-key-mode) 
  :diminish which-key-mode 
  :config (setq which-key-idle-delay 0.3))

;; rainbow delimiters
(use-package 
  rainbow-delimiters 
  :defer t 
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; paredit
(use-package 
  paredit 
  :defer t 
  :init (progn (add-hook 'emacs-lisp-mode-hook 'paredit-mode) 
               (add-hook 'clojure-mode-hook 'paredit-mode) 
               (add-hook 'clojurec-mode-hook 'paredit-mode) 
               (add-hook 'cider-repl-mode-hook 'paredit-mode)))

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

;; emacs help improvements
(use-package 
  helpful 
  :custom (counsel-describe-function-function #'helpful-callable) 
  (counsel-describe-variable-function #'helpful-variable) 
  :bind ([remap describe-function] . counsel-describe-function) 
  ([remap describe-command] . counsel-help-command) 
  ([remap describe-variable] . counsel-describe-variable) 
  ([remap describe-key] . helpful-key))

(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-SPC") nil)

(use-package 
  general 
  :config (general-evil-setup t) 
  (general-create-definer rr/leader-key 
    :keys '(normal inser visual emacs) 
    :prefix "C-SPC" 
    :global-prefix "C-SPC") 
  (general-create-definer rr/ctrl-c-keys 
    :prefix "C-c"))

(rr/leader-key "t" 
  '(:ignore t 
            :which-key "toggle")
  "tt" '(counsel-load-theme :which-key "choose theme"))

;; comment code efficiently
(use-package 
  evil-nerd-commenter 
  :bind ("M-/" . 'evilnc-comment-or-uncomment-lines) 
  ("C-c C-l" . 'evilnc-quick-comment-or-uncomment-to-the-line) 
  ("C-c C-c" . 'evilnc-copy-and-comment-lines) 
  ("C-c C-p" . 'evilnc-comment-or-uncomment-paragraphs))

;; edit mutiple regions
(use-package 
  iedit 
  :bind ("C-c ," . iedit-mode) 
  :diminish)

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

(use-package 
  elisp-format 
  :ensure t 
  :init)

;; find file in project
(use-package 
  find-file-in-project 
  :if (executable-find "fdfind") 
  :init (when (executable-find "fd") 
          (setq ffip-use-rust-fd t)) 
  :bind (("C-c o" . ffap) 
         ("C-c p" . ffip)))

;; search engine based on ripgrep
(use-package 
  ripgrep 
  :ensure t)

;; git diff
(use-package 
  git-gutter 
  :hook (prog-mode . git-gutter-mode) 
  :config (setq git-gutter:update-interval 0.02))

;; git diff enhanced
(use-package 
  git-gutter-fringe 
  :config (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated)) 
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated)) 
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; project manager
(use-package 
  projectile 
  :diminish projectile-mode 
  :config (projectile-mode) 
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

;; company
(use-package 
  company 
  :ensure t 
  :bind ("C-M-/" . company-complete-common-or-cycle) 
  :init (add-hook 'after-init-hook 'global-company-mode) 
  :config (setq company-show-quick-access t company-minimum-prefix-length 1 company-idle-delay 0.5
                company-backends '((company-files ; files & directory
                                    company-keywords ; keywords
                                    company-capf     ; what is this?
                                    company-yasnippet company-restclient) 
                                   (company-abbrev company-dabbrev))))

(use-package 
  company-box 
  :ensure t 
  :after company 
  :hook (company-mode . company-box-mode))

;; git
(use-package 
  magit 
  :bind ("C-M-;" . magit-status) 
  :commands (magit-status magit-get-current-branch) 
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; lsp - language server provider
(use-package 
  lsp-mode 
  :ensure t 
  :defer t 
  :commands (lsp lsp-deferred) 
  :bind (:map lsp-mode-map
              ("M-<RET>" . lsp-execute-action)) 
  :custom (lsp-auto-guess-root nil) 
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-enable-file-watchers nil) 
  (lsp-enable-folding nil) 
  (read-process-output-max (* 1024 1024)) 
  (lsp-keep-workspace-alive nil) 
  (lsp-eldoc-hook nil) 
  :hook ((c-mode . lsp) 
         (c++-mode . lsp) 
         (clojure-mode . lsp) 
         (rust-mode . lsp) 
         (lsp-mode . lsp-enable-which-key-integration)) 
  :config (setq lsp-keymap-prefix "C-c l") 
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map) 
  (setq lsp-file-watch-threshold 15000) 
  (setq lsp-ui-doc-enable nil) 
  (setq lsp-ui-doc-show-with-cursor nil) 
  (setq lsp-modeline-code-actions-enable nil) 
  (setq lsp-signature-render-documentation nil) 
  (setq lsp-lens-enable nil) 
  (setq lsp-enable-symbol-highlighting nil) 
  (setq lsp-eldoc-enable-hover nil) 
  (setq lsp-eldoc-hook nil) 
  (setq lsp-enable-links nil) 
  (setq lsp-log-io nil) 
  (setq lsp-enable-file-watchers nil) 
  (setq lsp-enable-on-type-formatting nil) 
  (setq lsp-completion-show-detail nil) 
  (setq lsp-completion-show-kind nil) 
  (setq lsp-headerline-breadcrumb-enable nil))

;; a hight level UI modules of lsp
(use-package 
  lsp-ui 
  :ensure t 
  :diminish 
  :defer t 
  :after lsp 
  :hook (lsp-mode . lsp-ui-mode) 
  :config (setq lsp-ui-sideline-enable t) 
  (setq lsp-ui-sideline-show-hover nil) 
  (setq lsp-ui-doc-position 'bottom) 
  (lsp-ui-doc-show) 
  :bind (:map lsp-ui-mode-map
              ("C-c i" . lsp-ui-menu)))

;; ivy integration
(use-package 
  lsp-ivy 
  :ensure t 
  :commands lsp-ivy-workspace-symbol)

;;  treemacs integration
(use-package 
  lsp-treemacs 
  :ensure t 
  :defer t 
  :after lsp)

;; debugger
(use-package 
  dap-mode 
  :after lsp-mode 
  :config (dap-auto-configure-mode) 
  :diminish 
  :bind (:map dap-mode-map
              (("<f12>" . dap-debug) 
               ("<f8>" . dap-continue) 
               ("<f9>" . dap-next) 
               ("<M-f11>" . dap-step-in) 
               ("C-M-<f11>" . dap-step-out) 
               ("<f7>" . dap-breakpoint-toggle))))

;; flycheck
(use-package 
  flycheck 
  :ensure t 
  :init)

;; clojure support
(use-package 
  flycheck-clj-kondo)

(use-package 
  clojure-mode 
  :after flycheck-clj-kondo 
  :config (require 'flycheck-clj-kondo))

;; ccls server
(use-package 
  ccls 
  :ensure t 
  :config 
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () 
                                                   (require 'ccls) 
                                                   (lsp))))

;; formatter
(use-package 
  clang-format 
  :ensure t 
  :init (add-hook 'c-mode-common-hook (function (lambda () 
                                                  (add-hook 'before-save-hook
                                                            'clang-format-buffer)))))
(use-package 
  geiser-mit 
  :ensure t 
  :config (setq geiser-mit-binary "/usr/bin/scheme" gbeiser-active-implementations '(mit)))

(global-set-key (kbd "M-o") 'ff-find-related-file)

;; cider clojure
(setq org-babel-clojure-backend 'cider)
(use-package 
  cider 
  :defer t 
  :init (progn (add-hook 'clojure-mode-hook 'cider-mode) 
               (add-hook 'clojurec-mode-hook 'cider-mode) 
               (add-hook 'cider-repl-mode-hook 'cider-mode)) 
  :config (setq cider-repl-display-help-banner nil) 
  (setq cider-auto-mode nil))

;; rust
(use-package 
  rust-mode 
  :defer t 
  :mode "\\.rs\\'" 
  :custom (rust-format-on-save t) 
  (lsp-rust-server 'rust-analyzer))

;; yaml support
(use-package 
  yaml-mode 
  :defer t)

;; dockerfile support
(use-package 
  dockerfile-mode 
  :defer t)

;; sh script support
(use-package 
  sh-script 
  :ensure nil 
  :config (with-eval-after-load 'company (add-hook 'sh-mode-hook #'(lambda () 
                                                                     (company-mode -1)))))

(use-package 
  shfmt 
  :config (add-hook 'sh-mode-hook 'shfmt-on-save-mode))

(use-package 
  fish-mode 
  :defer t 
  :mode "\\.fish\\'")

;; rest client
(use-package 
  restclient 
  :ensure t 
  :mode (("\\.http\\'" . restclient-mode)))

(use-package 
  company-restclient 
  :ensure t)

(use-package 
  page-break-lines)

;;
(use-package 
  exec-path-from-shell 
  :if (memq window-system '(mac ns x)) 
  :config (exec-path-from-shell-initialize))

;; terminal
(use-package 
  vterm
  :config (setq vterm-shell "/usr/bin/fish") 
  :hook (vterm-mode . (lambda () (hl-line-mode -1) (display-line-numbers-mode -1))))

(use-package
  multi-vterm
  :after vterm
  :ensure t
  :defer t
  :bind (("C-c t t" . #'multi-vterm)
         ("C-c t n" . #'multi-vterm-next)
         ("C-c t p" . #'multi-vterm-prev)))


;; org mode
(defun efs/org-mode-setup () 
  (org-indent-mode) 
  (variable-pitch-mode 1) 
  (visual-line-mode 1))

(defun efs/org-font-setup () 
  "Replace list hyphen with dot."
  (font-lock-add-keywords 
   'org-mode
   '(("^ *\\([-]\\) " (0 (prog1 () 
                           (compose-region (match-beginning 1) 
                                           (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2) 
                  (org-level-2 . 1.1) 
                  (org-level-3 . 1.05) 
                  (org-level-4 . 1.0) 
                  (org-level-5 . 1.1) 
                  (org-level-6 . 1.1) 
                  (org-level-7 . 1.1) 
                  (org-level-8 . 1.1))) 
    (set-face-attribute (car face) nil 
                        :font "Fira Code Retina" 
                        :weight 'regular 
                        :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil 
                      :foreground nil 
                      :inherit 'fixed-pitch) 
  (set-face-attribute 'org-code nil 
                      :inherit '(shadow fixed-pitch)) 
  (set-face-attribute 'org-table nil 
                      :inherit '(shadow fixed-pitch)) 
  (set-face-attribute 'org-verbatim nil 
                      :inherit '(shadow fixed-pitch)) 
  (set-face-attribute 'org-special-keyword nil 
                      :inherit '(font-lock-comment-face fixed-pitch)) 
  (set-face-attribute 'org-meta-line nil 
                      :inherit '(font-lock-comment-face fixed-pitch)) 
  (set-face-attribute 'org-checkbox nil 
                      :inherit 'fixed-pitch))

;; org mode
(use-package 
  org 
  :hook (org-mode . efs/org-mode-setup) 
  :config (setq org-ellipsis " ▾" org-hide-emphasis-markers t org-confirm-babel-evaluate nil
                org-fontify-quote-and-verse-blocks t org-startup-folded 'content
                org-agenda-start-with-log-mode t org-log-done 'time org-log-into-drawer t) 
  (efs/org-font-setup))

;; custom bullets
(use-package 
  org-bullets 
  :after org 
  :hook (org-mode . org-bullets-mode) 
  :custom (org-bullets-bullet-list '("◉" "○" "✸" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill () 
  (setq visual-fill-column-width 200 visual-fill-column-center-text t) 
  (visual-fill-column-mode 1))

(use-package 
  visual-fill-column 
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-babel-clojure-backend 'cider)

(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) 
                                                         (python . t) 
                                                         (shell . t) 
                                                         (clojure . t)))

;; org templates
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
(add-to-list 'org-structure-template-alist '("pyt" . "src python"))
(add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))

;; org-roam
(use-package 
  org-roam 
  :ensure t 
  :init (setq org-roam-v2-ack t) 
  :custom (org-roam-directory "~/Notes/Roam/") 
  (org-roam-completion-everywhere t) 
  (org-roam-capture-templates '(("d" "default" plain "%?" 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n") 
                                 :unnarrowed t) 
                                ("l" "programming language" plain (file
                                                                   "~/Notes/Roam/ProgrammingLanguagesTemplate.org") 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n") 
                                 :unnarrowed t) 
                                ("b" "book notes" plain (file "~/Notes/Roam/BookNotesTemplate.org") 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n") 
                                 :unnarrowed t) 
                                ("p" "project" plain (file "~/Notes/Roam/ProjectsTemplate.org") 
                                 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                    "#+title: ${title}\n#+filetags: Project") 
                                 :unnarrowed t))) 
  :bind (("C-c n l" . org-roam-buffer-toggle) 
         ("C-c n f" . org-roam-node-find) 
         ("C-c n i" . org-roam-node-insert) 
         :map org-mode-map ("C-M-i" . completion-at-point)) 
  :config (org-roam-setup))

(use-package
  elfeed-org
  :config (elfeed-org)
  :ensure t
  :custom (rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")) )

(use-package 
  dictionary 
  :bind (("C-c l" . dictionary-lookup-definition)) 
  :config (setq dictionary-server "dict.org"))

;; Configure Elfeed
(use-package 
  elfeed 
  :custom (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)) 
  (elfeed-show-entry-switch 'display-buffer) 
  :bind ("C-c w" . elfeed ))

(require 'init-functions)
(require 'init-eshell)

;; end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fussy eshell-git-prompt geiser-mit xterm-color eshell-toggle esh-autosuggest eshell-syntax-highlighting shfmt treemacs-all-the-icons magit org-roam visual-fill-column org-bullets dashboard yaml-mode which-key vterm-toggle use-package rust-mode ripgrep rainbow-delimiters perspective paredit page-break-lines minions lsp-ui lsp-ivy ivy-rich iedit helpful git-gutter-fringe general flycheck-clj-kondo find-file-in-project exec-path-from-shell evil-nerd-commenter evil-collection elisp-format doom-themes doom-modeline dockerfile-mode dired-single dired-ranger dired-rainbow dired-open dired-hide-dotfiles dired-collapse diminish dap-mode counsel-projectile company-restclient company-box clang-format cider ccls all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
