;; -*- coding: utf-8; lexical-binding: t -*-

;; Remove cl warnings
(setq byte-compile-warnings '(cl-functions))

;; startup fast
(setq gc-cons-threshold (* 50 1000 1000) read-process-output-max (* 1000 1000)
      treemacs-space-between-root-nodes nil company-idle-delay 0.0 company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

;; Profile emacs startup
(add-hook 'emacs-startup-hook (lambda () 
                                (message "*** Emacs loaded in %s with %d garbage collections."
                                         (format "%.2f seconds" (float-time (time-subtract
                                                                             after-init-time
                                                                             before-init-time)))
                                         gcs-done)))

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-message t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; no need to prompt for the read command _every_ time
 compilation-read-command nil
 ;; scroll to first error
 compilation-scroll-output 'first-error
 ;; accept 'y' or 'n' instead of yes/no
 use-short-answers t
 ;; eke out a little more scrolling performance
 fast-but-imprecise-scrolling t
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; when I say to quit, I mean quit
 confirm-kill-processes nil
 ;; if native-comp is having trouble, there's not very much I can do
 native-comp-async-report-warnings-erros 'silent
 ;; unicode ellipses are better
 truncate-string-ellipsis "...")

;; never mix tabs and spaces. Never use tabs, period.
(setq-default indent-tabs-mode nil)

;; enabling line numbers
(delete-selection-mode t)
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook)) 
  (add-hook mode (lambda () 
                   (display-line-numbers-mode 0))))

;; ignore backup files
(setq make-backup-files nil auto-save-default nil create-lockfiles nil)

;; gui enhacements
(when (window-system) 
  (scroll-bar-mode -1) 
  (tool-bar-mode -1) 
  (tooltip-mode -1) 
  (set-fringe-mode 10) 
  (global-hl-line-mode +1) 
  (menu-bar-mode -1) 
  (auto-revert-mode 1))

(set-frame-parameter (selected-frame) 'alpha '(95 . 70))
(add-to-list 'default-frame-alist '(alpha . (95 . 70)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; font configuration
(set-face-attribute 'default nil 
                    :font "Fira Code Retina" 
                    :height 80)

;; changing cursor type.
(setq-default cursor-type 'bar)

(setq custom-file (make-temp-name "/tmp/"))

;; Disable C-z
(global-unset-key (kbd "C-z"))

;; ESC cancels all commands
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; eval buffer
(global-set-key (kbd "C-x C-e") 'eval-buffer)

;; elisp format
(global-set-key (kbd "C-c e f") 'elisp-format-buffer)

;; move cursor
(global-set-key (kbd "C-c b b") 'beginning-of-buffer)
(global-set-key (kbd "C-c b e") 'end-of-buffer)

;; custom window management
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

(defun pt/split-window-two () 
  "Split a window into two." 
  (interactive) 
  (split-window-right) 
  (balance-windows))


(global-set-key (kbd "C-c 2") #'pt/split-window-two)

(defun open-init-file () 
  "Open config file file." 
  (interactive) 
  (find-file "~/.emacs"))

(global-set-key (kbd "C-c i") #'open-init-file)

;; changing yes or no question
(defalias 'yes-or-no-p 'y-or-n-p)

;; set encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
(global-set-key (kbd "s-w") #'kill-this-buffer)

;; ********** Config packges

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/") 
                         ("melpa-stable" . "https://stable.melpa.org/packages/") 
                         ("org" . "https://orgmode.org/elpa/") 
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


(setq rar/exwm-enabled (and (eq window-system 'x) 
                            (seq-contains command-line-args "--use-exwm")))

(when rar/exwm-enabled 
  (require 'exwm) 
  (require 'exwm-config) 
  (exwm-config-default))

;; Theme
(use-package 
  doom-themes 
  :ensure t 
  :config (setq doom-themes-enable-bold t doom0themes-enable-italic t) 
  (load-theme 'doom-dracula t))

;; Rainbow delimiters
(use-package 
  paren 
  :config (show-paren-mode) 
  :custom (show-paren-style 'parenthesis))
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil 
                    :weight 'extra-bold)

(use-package 
  rainbow-delimiters 
  :ensure t 
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package 
  smartparens 
  :diminish smartparens-mode 
  :init (smartparens-global-mode) 
  :config (require 'smartparens-config))

(use-package 
  iedit 
  :ensure t)

(use-package 
  sudo-edit)

;; Key helper
(use-package 
  which-key 
  :init (which-key-mode) 
  :diminish which-key-mode 
  :config (setq which-key-idle-delay 0.3))

;; Enhanced completition framework
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

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist) 
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15) 
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15) 
  (setf (alist-get 'swiper ivy-height-alist) 15) 
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package 
  ivy-hydra 
  :defer t 
  :after hydra)

(use-package 
  counsel 
  :bind (("M-x" . counsel-M-x) 
         ("C-x b" . counsel-ibuffer) 
         ("C-x C-f" . counsel-find-file) 
         ("C-c R" . counsel-rg) 
         ("C-c F" . counsel-fzf) 
         ("C-M-j" . counsel-switch-buffer) 
         ("M-y" . counsel-yank-pop) 
         :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history)) 
  :config (setq ivy-initial-inputs-alias nil) 
  (setq counsel-find-file-ignore-regexp ".*cache"))

(use-package 
  ivy-rich 
  :after ivy 
  counsel 
  :init (ivy-rich-mode 1))

(use-package 
  helpful 
  :custom (counsel-describe-function-function #'helpful-callable) 
  (counsel-describe-variable-function #'helpful-variable) 
  :bind ([remap describe-function] . counsel-describe-function) 
  ([remap describe-command] . counsel-help-command) 
  ([remap describe-variable] . counsel-describe-variable) 
  ([remap describe-key] . helpful-key))

;; Modeline theme
(use-package 
  all-the-icons 
  :if (display-graphic-p))

(use-package 
  minions 
  :hook (doom-modeline-mode . minions-mode))

(use-package 
  doom-modeline 
  :ensure t 
  :init (doom-modeline-mode 1) 
  :custom ((doom-modeline-height 15)))

(use-package 
  undo-tree 

  :diminish 
  :init (global-undo-tree-mode +1) 
  :bind (("C-c _" . undo-tree-visualize) 
         ("C-c z" . undo-tree-undo) 
         ("C-c r" . undo-tree-redo)) 
  :config (unbind-key "M-_" undo-tree-map) 
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package 
  elisp-format 
  :ensure t 
  :init)

;; c/c++ format
(use-package 
  clang-format+ 
  :init)
(add-hook 'c-mode-common-hook #'clang-format+-mode)

;; switch to header and source
(defun dts-switch-between-header-and-source () 
  (interactive) 
  (setq bse (file-name-sans-extension buffer-file-name)) 
  (setq ext (downcase (file-name-extension buffer-file-name))) 
  (cond ((or 
          (equal ext "h") 
          (equal ext "hpp")) 
         (setq nfn (concat bse ".c")) 
         (if (file-exists-p nfn) 
             (find-file nfn) 
           (progn 
             (setq nfn (concat bse ".cpp")) 
             (find-file nfn)))) 
        ((or 
          (equal ext "cpp") 
          (equal ext "c")) 
         (setq nfn (concat bse ".h")) 
         (find-file nfn))))

(global-set-key (kbd "C-c s") 'dts-switch-between-header-and-source)

(use-package 
  hydra 
  :defer 1)

(defhydra hydra-text-scale 
  (:timeout 4)
  "scale text" ("j" text-scale-increase "in") 
  ("k" text-scale-decrease "out") 
  ("f" nil "finished" 
   :exit t))

;; Project manager - Projectile
(defun switch-project-action () 
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name)) 
  (magit-status))

(use-package 
  projectile 
  :diminish projectile-mode 
  :config (projectile-mode) 
  :demand t 
  :bind-keymap ("C-c p" . projectile-command-map) 
  :init (when (file-directory-p "~/Projetos") 
          (setq projectile-project-search-path '("~/Projetos"))) 
  (setq projectile-switch-project-action #'switch-project-action))

(use-package 
  counsel-projectile 
  :after projectile 
  :bind (("C-M-p" . counsel-projectile-find-file)) 
  :config (counsel-projectile-mode))

(use-package 
  perspective 
  :init (setq persp-suppress-no-prefix-key-warning t) 
  :demand t 
  :bind (("C-M-k" . persp-switch) 
         ("C-M-n" . persp-next) 
         ("C-x k" . persp-kill-buffer*)) 
  :custom (persp-initial-frame-name "Main") 
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t) 
    (persp-mode)))

;; Git support
(use-package 
  magit 
  :bind ("C-M-;" . magit-status) 
  :commands (magit-status magit-get-current-branch) 
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package 
  git-gutter 
  :hook (prog-mode . git-gutter-mode) 
  :config (setq git-gutter:update-interval 0.02))

(use-package 
  git-gutter-fringe 
  :config (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated)) 
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated)) 
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(add-hook 'before-save-hook (lambda () 
                              (when (eq major-mode 'c++-mode) 
                                (set-buffer-file-coding-system 'iso-8859-1) 
                                (message "Covertido"))))

;; Language server provider
(use-package 
  lsp-mode 
  :ensure t 
  :commands (lsp lsp-deferred) 
  :hook ((c-mode . lsp) 
         (c++-mode . lsp) 
         (lsp-mode . lsp-enable-which-key-integration)) 
  :config (setq lsp-keymap-prefix "C-c l") 
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map) 
  (setq lsp-file-watch-threshold 15000))

(setq lsp-ui-doc-enable nil)
(setq lsp-lens-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)

(use-package 
  lsp-ui 
  :ensure t 
  :after lsp 
  :hook (lsp-mode . lsp-ui-mode) 
  :config (setq lsp-ui-sideline-enable t) 
  (setq lsp-ui-sideline-show-hover nil) 
  (setq lsp-ui-doc-position 'bottom) 
  (lsp-ui-doc-show))

(setq lsp-diagnostics-provider 
      :none)

(use-package 
  lsp-ivy 
  :ensure t 
  :commands lsp-ivy-workspace-symbol)

;; company
(use-package 
  company 
  :ensure t 
  :bind ("C-M-/" . company-complete-common-or-cycle) 
  :init (add-hook 'after-init-hook 'global-company-mode) 
  :config (setq company-show-numbers t company-minimum-prefix-length 1 company-idle-delay 0.5
                company-backends '((company-files ; files & directory
				    company-keywords ; keywords
				    company-capf     ; what is this?
				    company-yasnippet) 
                                   (company-abbrev company-dabbrev))))

(use-package 
  company-box 
  :ensure t 
  :after company 
  :hook (company-mode . company-box-mode))

(use-package 
  lsp-treemacs 
  :ensure t 
  :commands lsp-treemacs-erros-list)

(use-package 
  yasnippet 
  :hook (prog-mode . yas-minor-mode) 
  :config (yas-reload-all))

;; Build system
(defun bearmake-compile-command () 
  "Bear make compile command." 
  (interactive) 
  (set (make-local-variable 'compile-command) 
       (concat "make distclean -j6 && bear make -C src -j6 && make -C mock -j6 && make -C test -j6"
               (if buffer-file-name (shell-quote-argument (file-name-sans-extension
                                                           buffer-file-name))))) 
  (call-interactively 'compile))

(defhydra hydra-build 
  (:timeout 4)
  "compile" ("B" bearmake-compile-command "Bear Make"))

(use-package 
  modern-cpp-font-lock 
  :ensure t 
  :config (modern-c++-font-lock-global-mode t))

;; Go Lang
(use-package 
  go-mode 
  :defer t 
  :hook (go-mode . lsp-deferred) 
  :config (add-hook 'before-save-hook #'gofmt-before-save))

(use-package 
  go-snippets 
  :defer t)

(defun fix-messed-up-gofmt-path () 
  (interactive) 
  (setq gofmt-command (string-trim (shell-command-to-string "which gofmt"))))

(use-package 
  gotest 
  :bind (:map go-mode-map
              ("C-c a t" . #'go-test-current-test)))

;; Rust
(use-package 
  rust-mode 
  :defer t 
  :custom (rust-format-on-save t) 
  (lsp-rust-server 'rust-analyzer))

;; clojure
(use-package 
  cider 
  :mode "\\.clj[sc]?\\'" 
  :config)

(use-package 
  yaml-mode 
  :defer t)
(use-package 
  dockerfile-mode 
  :defer t)
(use-package 
  toml-mode 
  :defer t)
(use-package 
  asn1-mode 
  :defer t)

;; RSS reader
(use-package 
  elfeed 
  :ensure t 
  :commands elfeed 
  :config (setq elfeed-feeds '("https://www.fluentcpp.com/feed"
                               "https://www.reddit.com/r/emacs/.rss"
                               "https://blog.rust-lang.org/feed.xml"
                               "https://www.reddit.com/r/golang/.rss"
                               "https://golangnews.com/index.xmle"
                               "https://www.reddit.com/r/EmuDev/.rss"
                               "https://www.reddit.com/r/rust/.rss"
                               "https://clojure-diary.gitlab.io/feed.xml"
                               "https://www.reddit.com/r/Clojure/.rss") elfeed-db-directory
                               (expand-file-name "elfeed" user-emacs-directory)) 
  :bind ("C-x w" . elfeed))

;; Nerd commenter
(use-package 
  evil-nerd-commenter 
  :bind ("M-/" . 'evilnc-comment-or-uncomment-lines) 
  ("C-c C-l" . 'evilnc-quick-comment-or-uncomment-to-the-line) 
  ("C-c C-c" . 'evilnc-copy-and-comment-lines) 
  ("C-c C-p" . 'evilnc-comment-or-uncomment-paragraphs))

(use-package 
  all-the-icons-dired 
  :after all-the-icons 
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package 
  dired 
  :ensure nil 
  :defer 1 
  :commands (dired dired-jump) 
  :bind ("C-x C-j" . dired-jump) 
  :config (autoload 'dired-omit-mode "dired-x") 
  (add-hook 'dired-load-hook (lambda () 
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
  exec-path-from-shell 
  :ensure t 
  :if (and (equal system-type 'darwin) 
           (window-system)) 
  :custom (setq exec-path-from-shell-check-startup-files nil) 
  (setq exec-path-from-shell-variables . '("PATH" "GOPATH")) 
  :config (exec-path-from-shell-initialize))


(use-package 
  vterm 
  :config (defun turn-off-chrome () 
            (hl-line-mode -1) 
            (display-line-numbers-mode -1)) 
  :hook (vterm-mode . turn-off-chrome))

(use-package 
  vterm-toggle 
  :custom (vterm-toggle-fullscreen-p nil "Open a vterm in another window.") 
  (vterm-toggle-scope 'project) 
  :bind (("C-c t" . #'vterm-toggle) :map vterm-mode-map ("s-t" . #'vterm) ; Open up new tabs quickly
         ("s-v" . #'vterm-yank)))
