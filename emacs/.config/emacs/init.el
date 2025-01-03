;;; gc
(setq gc-cons-threshold (* 50 1000 1000))

;;; profile
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "*** Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
            (float-time
             (time-subtract after-init-time before-init-time)))
    gcs-done)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(when (eq (string-match "d728970d49a1" system-name) 0)
  (message "Changing to latin1")
  (prefer-coding-system 'iso-8859-1)
  (set-default-coding-systems 'iso-8859-1)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

;;; Tweak the looks of Emacs
;; general variables
(setq
 inhibit-startup-message t ; no welcome buffer
 initial-scratch-message ";;scratch buffer\n\n" ; scratch buffer text
 ring-bell-function 'ignore ; never ding
 history-length 20 ; max history saves
 use-dialog-box nil ; no ugly dialogs
 case-fold-search nil ; case sensitive search
 confirm-kill-processes nil ; just quit
 global-auto-revert-non-file-buffers t ; update buffers thar are non-files too
 sentence-end-double-space nil ; no way double spaces
 load-prefer-newer t ; always load the new file
 tab-always-indent 'complete ; use TAB to complete symbols
 native-comp-async-report-warnings-erros 'silent ; there's not very much I can do
 mouse-wheel-scroll-amount '(2 ((shift) . 1)) ; scroll 2 lines
 mouse-wheel-progressive-speed nil ; don't accelerate
 mouse-wheel-follow-mouse 't ; scroll window under mouse cursor
 vc-follow-symlinks t ; goto the real file
 show-paren-style 'mixed ;highlight the matching paren
 )

(setq
 backup-directory-alist '(("." . "~/.emacs.d/backup"))
 backup-by-copying t ; Don't delink hardlinks
 version-control t ; Use version numbers on backups
 delete-old-versions t ; Automatically delete excess backups
 kept-new-versions 20 ; how many of the newest versions to keep
 kept-old-versions 5
 display-line-numbers-type 't
 dired-kill-when-opening-new-dired-buffer t) ; and how many of the old

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
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(xterm-mouse-mode +1)
(winner-mode +1)

;; yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; Save and revert operations

;; auto-save file name conversion.
(setq auto-save-file-name-transforms
      `((".*"
         ,(expand-file-name "auto-save-list" user-emacs-directory)
         t)))

;; Auto save buffer if idled for 2 seconds.
(setq auto-save-timeout 2)
(auto-save-visited-mode 1)

;; Watch and reload the file changed on the disk.
(setq auto-revert-remote-files t)
(global-auto-revert-mode 1)

;; Do not generate any messages.
(setq auto-revert-verbose nil)

;; Do not create lock files (prefix ".#").
(setq create-lockfiles nil)

;; disable line numbers for some modes
(dolist (mode
         '(org-mode-hook
           term-mode-hook
           vterm-mode-hook
           eat-mode-hook
           shell-mode-hook
           eshell-mode-hook
           dired-mode-hook
           pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;;; Set up the package manager
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; install package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list
 'display-buffer-alist
 '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
   (display-buffer-no-window)
   (allow-no-window . t)))

;;; Basic behaviour
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package diminish :ensure t)

(use-package
 delsel
 :ensure nil
 :hook (after-init . delete-selection-mode))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(let ((mono-spaced-font "JetBrainsMono Nerd Font")
      (proportionately-spaced-font "JetBrainsMono Nerd Font"))
  (set-face-attribute 'default nil
                      :family mono-spaced-font
                      :height 100)
  (set-face-attribute 'fixed-pitch nil
                      :family mono-spaced-font
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family proportionately-spaced-font
                      :height 1.0))

(use-package
 modus-themes
 :ensure t
 :custom
 (modus-themes-italic-constructs t)
 (modus-themes-bold-constructs t)
 (modus-vivendi-tinted-palette-overrides ;; doom-moonlight
  '((bg-main "#212337")
    (fg-main "#c8d3f5")
    (fg-active fg-main)
    (bg-hl-line "#2f334d")
    (fg-mode-line-active "#b4c2f0")
    (bg-mode-line-active "#1e2030")
    (fg-mode-line-inactive "#bac2de")
    (bg-mode-line-inactive "#313244")
    (border-mode-line-active bg-mode-line-active)
    (border-mode-line-inactive bg-mode-line-inactive)
    (bg-line-number-active line-highlight)
    (fg-line-number-active fg-main)
    (bg-line-number-inactive "#212337")
    (fg-line-number-inactive "#444a73")
    (bg-region "#383e5c")
    (fg-region fg-main)
    (bg-completion bg-region)
    (fg-completion fg-main)
    (fg-prompt "#c099ff")
    (bg-prompt unspecified)
    (cursor "#baacff")

    (rainbow-0 "#c099ff")
    (rainbow-1 "#ff995e")
    (rainbow-2 "#ff98a4")
    (rainbow-3 "#b4f9f8")
    (rainbow-4 "#f989d3")
    (rainbow-5 "#ffc777")
    (rainbow-6 "#82aaff")
    (rainbow-7 "#4fd6be")
    (rainbow-8 "#86e1fc")
    (accent-0 "#82aaff")

    (keyword "#c099ff")
    (builtin "#c099ff")
    (comment "#7a88cf")
    (string "#c3e88d")
    (fnname "#82aaff")
    (name fnname)
    (type "#ffc777")
    (variable "#ff98a4")
    (docstring "#7a88cf")
    (constant "#ff995e")
    (number "#ff995e")
    (err "#ff757f")
    (warning "#ffc777")
    (info "#b4f9f8")))
 (modus-operandi-deuteranopia-palette-overrides
  '((bg-main "#eff1f5")
    (fg-main "#4c4f69")
    (border-mode-line-active "#303446")
    (border-mode-line-inactive "#303446")))
 :config (load-theme 'modus-vivendi-tinted :no-confirm-loading))

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons :ensure t)

(use-package
 nerd-icons-completion
 :ensure t
 :after marginalia
 :config
 (add-hook
  'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package
 nerd-icons-corfu
 :ensure t
 :after corfu
 :config
 (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package
 nerd-icons-dired
 :ensure t
 :hook (dired-mode . nerd-icons-dired-mode))

;;; Configure the minibuffer and completions
(use-package
 vertico
 :bind
 (:map
  vertico-map
  ("C-j" . vertico-next)
  ("C-k" . vertico-previous)
  ("C-f" . vertico-exit)
  :map
  minibuffer-local-map
  ("M-h" . backward-kill-word))
 :custom (vertico-cycle t)
 :init (vertico-mode))

(use-package
 marginalia
 :after vertico
 :custom
 (marginalia-annotators
  '(marginalia-annotators-heavy marginalia-annotators-ligh nil))
 :init (marginalia-mode))

(use-package
 embark
 :after vertico
 :ensure t
 :bind
 (("C-c ." . embark-act) ; pick some comfortable binding
  ("C-c ;" . embark-dwim) ; good alternative: M-.
  ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
 :init
 ;; Optionally replace the key help with a completing-read interface
 (setq prefix-help-command #'embark-prefix-help-command)
 :config
 ;; Hide the mode line of the Embark live/completions buffers
 (add-to-list
  'display-buffer-alist
  '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    nil
    (window-parameters (mode-line-format . none)))))

(use-package
 embark-consult
 :ensure t ; only need to install it, embark loads it after consult if found
 :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package
 consult
 :bind
 (("C-s" . 'consult-line)
  ("C-x b" . 'consult-buffer)
  ("C-x C-b" . 'consult-buffer)
  ("M-g o" . 'consult-outline)
  ("M-s g" . 'consult-grep)
  ("M-s r" . 'consult-ripgrep))
 :custom
 (completion-in-region-function #'consult-completion-in-region))

(use-package
 orderless
 :ensure t
 :config
 (setq completion-styles '(orderless basic))
 (setq completion-category-defaults nil)
 (setq completion-category-overrrides nil))

(use-package
 savehist
 :ensure nil ; it is built-in
 :hook (after-init . savehist-mode))

(use-package
 corfu
 :ensure t
 :hook (after-init . global-corfu-mode)
 :bind (:map corfu-map ("<tab>" . corfu-complete))
 :config
 (setq tab-always-indent 'complete)
 (setq corfu-preview-current nil)
 (setq corfu-min-width 20)

 (setq corfu-popupinfo-delay '(1.25 . 0.5))
 (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

 ;; Sort by input history (no need to modify `corfu-sort-function').
 (with-eval-after-load 'savehist
   (corfu-history-mode 1)
   (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package
 corfu-terminal
 :config
 (unless (display-graphic-p)
   (corfu-terminal-mode +1)))

(use-package wgrep :after consult :hook (grep-mode . wgrep-setup))

(use-package
 consult-dir
 :bind
 (("C-x C-d" . consult-dir)
  :map
  vertico-map
  ("C-x C-d" . consult-dir)
  ("C-x C-j" . consult-dir-jump-file))
 :custom (consult-dir-project-list-function nil))

(use-package vertico-posframe
  :ensure t
  :config
  (setq vertico-posframe-mode 1))

;;; Helpers
(use-package
 which-key
 :init (which-key-mode)
 :diminish which-key-mode
 :config (setq which-key-idle-delay 0.3))

(use-package
 helpful
 :init
 :defer t
 :bind
 ([remap describe-function] . helpful-function)
 ([remap describe-command] . helpful-command)
 ([remap describe-variable] . helpful-variable)
 ([remap describe-key] . helpful-key))

;; Text and code complement
(use-package
 company
 :ensure t
 :bind ("C-M-/" . company-complete-common-or-cycle)
 :diminish
 :init (global-company-mode)
 :config
 (setq
  company-show-quick-access t
  company-minimum-prefix-length 2
  company-idle-delay 0.5
  company-show-numbers t
  company-tooltip-align-annotations t
  company-begin-commands '(self-insert-command)
  company-backends
  '((company-files ; files & directory
     company-keywords ; keywords
     company-capf ; what is this?
     company-yasnippet)
    (company-abbrev company-dabbrev))))

(use-package
 company-box
 :ensure t
 :after company
 :hook (company-mode . company-box-mode))

;;; The file manager (Dired)
(use-package
 dired
 :ensure nil
 :commands (dired)
 :hook
 ((dired-mode . dired-hide-details-mode) (dired-mode . hl-line-mode))
 :config
 (setq dired-recursive-copies 'always)
 (setq dired-recursive-deletes 'always)
 (setq delete-by-moving-to-trash t)
 (setq dired-dwim-target t))

(use-package
 dired-subtree
 :ensure t
 :after dired
 :bind
 (:map
  dired-mode-map
  ("<tab>" . dired-subtree-toggle)
  ("TAB" . dired-subtree-toggle)
  ("<backtab>" . dired-subtree-remove)
  ("S-TAB" . dired-subtree-remove))
 :config (setq dired-subtree-use-backgrounds nil))

(use-package
 trashed
 :ensure t
 :commands (trashed)
 :config
 (setq trashed-action-confirmer 'y-or-n-p)
 (setq trashed-use-header-line t)
 (setq trashed-sort-key '("Date deleted" . t))
 (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Named workspaces
(use-package
 perspective
 :demand t
 :bind
 (("C-M-k" . persp-switch)
  ("C-M-n" . persp-next)
  ("C-x k" . persp-kill-buffer*))
 :custom
 (persp-initial-frame-name "Main")
 (persp-mode-prefix-key (kbd "C-c M-p"))
 :config
 ;; Running `persp-mode' multiple times resets the perspective list...
 (unless (equal persp-mode t)
   (persp-mode)))

;;; Projects
(defun remacs/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package
 projectile
 :diminish projectile-mode
 :config (projectile-mode)
 (setq projectile-globally-ignored-directories
       (append
        '(".git" ".ccls_cache")
        projectile-globally-ignored-directories))
 :demand t
 :bind-keymap ("C-c p" . projectile-command-map)
 :init
 (when (file-directory-p "~/dev/personal")
   (setq projectile-project-search-path '("~/dev/personal")))
 (setq projectile-switch-project-action
       #'remacs/switch-project-action))

;; Git support
(use-package
 magit
 :bind ("C-M-;" . magit-status)
 :commands (magit-status magit-get-current-branch)
 :custom
 (magit-display-buffer-function
  #'magit-display-buffer-same-window-except-diff-v1))

(use-package
 git-gutter
 :hook (prog-mode . git-gutter-mode)
 :config (setq git-gutter:update-interval 0.02))


;; Modeline and themes
(use-package minions :config (minions-mode 1))

(use-package
 doom-themes
 :ensure t
 :config
 (setq
  doom-themes-enable-bold t
  doom-themes-enable-italic t)
 ;;(load-theme 'doom-dracula t)
 (doom-themes-org-config) (doom-themes-neotree-config))

;; Undo/redo framework
(use-package
 undo-fu
 :ensure t
 :config
 (global-set-key (kbd "C-/") 'undo-fu-only-undo)
 (global-set-key (kbd "C-M-/") 'undo-fu-only-redo))

(use-package
 undo-fu-session
 :ensure t
 :config
 (setq undo-fu-session-incompatible-files
       '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
 (global-undo-fu-session-mode))

;; comment code efficiently
(use-package
 evil-nerd-commenter
 :bind ("M-/" . 'evilnc-comment-or-uncomment-lines))

;;;; Programming
;; LSP
(use-package
 lsp-mode
 :after company
 :ensure t
 :defer t
 :commands (lsp lsp-deferred)
 :bind (:map lsp-mode-map ("M-<RET>" . lsp-execute-action))
 :custom
 (lsp-auto-guess-root nil)
 (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
 (lsp-enable-file-watchers nil)
 (lsp-enable-folding nil)
 (read-process-output-max (* 1024 1024))
 (lsp-keep-workspace-alive nil)
 (lsp-eldoc-hook nil)
 :hook
 ((c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (clojure-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))
 :config
 (setq lsp-keymap-prefix "C-c l")
 (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
 (setq lsp-clients-clangd-executable "/usr/bin/clangd")
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
 :config
 (setq lsp-ui-sideline-enable t)
 (setq lsp-ui-sideline-show-hover nil)
 (setq lsp-ui-doc-position 'bottom)
 (lsp-ui-doc-show)
 :bind (:map lsp-ui-mode-map ("C-c i" . lsp-ui-menu)))

(use-package consult-lsp :init :defer t :after lsp)

;; C++ language server
(use-package
 ccls
 :ensure t
 :config
 :hook
 ((c-mode c++-mode objc-mode cuda-mode)
  .
  (lambda ()
    (require 'ccls)
    (lsp))))

;; C++ Formatter
(use-package
 clang-format
 :ensure t
 :config
 (add-hook
  'c-mode-common-hook
  (lambda ()
    (add-hook
     (make-local-variable 'before-save-hook) 'clang-format-buffer))))

(use-package
 modern-cpp-font-lock
 :ensure t
 :hook (c++-mode . modern-c++-font-lock-mode))

;; Rust
(use-package
 rust-mode
 :defer t
 :mode "\\.rs\\'"
 :custom
 (rust-format-on-save t)
 (lsp-rust-server 'rust-analyzer))

;;; markdown
(use-package
 markdown-mode
 :mode "\\.md\\'"
 :config
 (setq markdown-command "pandoc")
 (setq markdown-asymmetric-header t)
 (setq markdown-header-scaling t)
 (setq markdown-enable-math t)
 :bind
 (:map markdown-mode-map ("M-<left>" . markdown-promote))
 (:map markdown-mode-map ("M-<right>" . markdown-demote))
 (:map markdown-mode-map ("M-S-<left>" . markdown-promote-subtree))
 (:map markdown-mode-map ("M-S-<right>" . markdown-demote-subtree)))

(use-package markdown-preview-mode :commands markdown-preview)

;; Elisp
(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package
 eldoc
 :defer t
 :after company
 :init
 (eldoc-add-command
  'company-complete-selection
  'company-complete-common
  'company-capf
  'company-abort))

;; shell
(use-package
 shell
 :defer t
 :init
 :hook (shell-mode . tree-sitter-hl-mode)
 :config
 (add-hook
  'after-save-hook
  'executable-make-buffer-file-executable-if-script-p))

;; ssh
(use-package ssh-config-mode :defer t)

;; direnv
(use-package
  envrc
  :hook (after-init . envrc-global-mode))

;; toml
(use-package
 toml-mode
 :init
 :defer t
 :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

;; yaml
(use-package
 yaml-mode
 :defer t
 :init
 :mode "\\.yml\\'"
 :mode "\\.yaml\\'")

;; python
(use-package
 python-mode
 :ensure t
 :hook (python-mode . lsp-deferred)
 :custom (python-shell-interpreter "python"))

;; pyenv
(use-package pyvenv :config (pyvenv-mode 1))

;; clojure check
(use-package flycheck-clj-kondo)

;; clojure mode
(use-package
 clojure-mode
 :after flycheck-clj-kondo
 :config (require 'flycheck-clj-kondo))

;; cider clojure
(setq org-babel-clojure-backend 'cider)
(use-package
 cider
 :defer t
 :init
 (progn
   (add-hook 'clojure-mode-hook 'cider-mode)
   (add-hook 'clojurec-mode-hook 'cider-mode)
   (add-hook 'cider-repl-mode-hook 'cider-mode))
 :config
 (setq cider-repl-display-help-banner nil)
 (setq cider-auto-mode nil))

;; scheme
(use-package
 geiser-guile
 :ensure t
 :config
 (setq scheme-program-name "guile")
 (setq geiser-default-implementation 'guile)
 (setq geiser-active-implementations '(guile))
 (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
 (setq geiser-guile-binary "guile")
 (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
 (add-hook 'inferior-scheme-mode-hook 'rainbow-delimiters-mode))

(use-package nix-mode :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode :ensure nix-mode :mode "\\.drv\\'")

(use-package
 nix-shell
 :ensure nix-mode
 :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl :ensure nix-mode :commands (nix-repl))

;; syntax check
(use-package
  flycheck
  :ensure t
  :init
  :hook
  (sh-mode . (lambda () (flycheck-mode))))

(use-package
  shfmt
  :defer t
  :hook
  (sh-mode . (lambda () (shfmt-on-save-mode))))

;; Terminals
;; vterm
(use-package
 vterm
 :hook
 (vterm-mode . (lambda ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))))

;; eat
(use-package eat
  :custom
  (eat-term-name "xterm-256color")
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
:hook
 (vterm-mode . (lambda ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))))

(defun remacs/eshell-config ()
  "Eshell config"
  (add-hook
   'eshell-pre-command-hook
   (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook
   'eshell-post-command-hook (lambda () (setenv "TERM" "dumb")))
  (setq
   eshell-prompt-function 'eshell-prompt
   eshell-highlight-prompt nil ;;
   eshell-buffer-shorthand t ;;
   eshell-history-size 5000 ;;
   eshell-buffer-maximum-lines 12000 ;; truncate after 12k lines
   eshell-hist-ignoredups t ;; ignore duplicates
   eshell-error-if-no-glob t ;;
   eshell-glob-case-insensitive t ;;
   eshell-scroll-to-bottom-on-input 'all ;;
   eshell-list-files-after-cd t ;;
   eshell-aliases-file (concat user-emacs-directory "eshell/alias") ;;
   eshell-banner-message "" ;; welcome message
   )

  (setq eshell-visual-commands
        '("vim"
          "nvim"
          "screen"
          "top"
          "less"
          "more"
          "lynx"
          "ncftp"
          "pine"
          "tin"
          "trn"
          "elm"
          "vim"
          "nmtui"
          "alsamixer"
          "htop"
          "el"
          "elinks"
          "btm"))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq
     eshell-highlight-prompt nil
     eshell-prompt-function 'epe-theme-multiline-with-status)))

(use-package eshell-prompt-extras :ensure t :after eshell)

(use-package eshell :config (remacs/eshell-config))

;;(setenv "EXA_COLORS" "uu=36:gu=37:sn=32:sb=32:da=34:ur=34:uw=35:ux=36:ue=36:gr=34:gw=35:gx=36:tr=34:tw=35:tx=36:")

(use-package
 eshell-syntax-highlighting
 :after esh-mode
 :config (eshell-syntax-highlighting-global-mode +1))

;; Programming enhacements
(use-package iedit :bind ("C-;" . iedit-mode) :diminish)

(use-package
 rainbow-delimiters
 :defer t
 :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package
 smartparens
 :ensure t
 :init
 (require 'smartparens-config)
 (smartparens-global-mode t)
 :diminish smartparens-mode
 :config (show-smartparens-mode t))

(use-package highlight-parentheses :ensure t)

;; Buffer search
(use-package
 visual-replace
 :defer t
 :bind
 (("C-c r" . visual-replace)
  :map
  isearch-mode-map
  ("C-c r" . visual-replace-from-isearch)))

;; Authentication
(use-package
 pinentry
 :config (setq epg-pinentry-mode 'loopback) (pinentry-start))

;; Templates
(use-package
 tempel
 :custom (tempel-trigger-prefix "<")
 :bind
 (("M-+" . tempel-complete) ;; Alternative tempel-expand
  ("M-*" . tempel-insert))
 :init
 (defun tempel-setup-capf ()
   (setq-local completion-at-point-functions
               (cons #'tempel-expand completion-at-point-functions)))

 (add-hook 'conf-mode-hook 'tempel-setup-capf)
 (add-hook 'prog-mode-hook 'tempel-setup-capf)
 (add-hook 'text-mode-hook 'tempel-setup-capf)

 ;; Optionally make the Tempel templates available to Abbrev,
 ;; either locally or globally. `expand-abbrev' is bound to C-x '.
 (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
 ;; (global-tempel-abbrev-mode)
 )

(use-package
  tempel-collection
  :ensure t
  :after tempel)

(use-package
  sxhkdrc-mode
  :ensure t)

;; Orgmode
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  "Replace list hyphen with dot."
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) " (0 (prog1 ()
           (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face
           '((org-level-1 . 1.2)
             (org-level-2 . 1.1)
             (org-level-3 . 1.05)
             (org-level-4 . 1.0)
             (org-level-5 . 1.1)
             (org-level-6 . 1.1)
             (org-level-7 . 1.1)
             (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
                        :font "RobotoMono Nerd Font"
                        :weight 'regular
                        :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil
                      :foreground "unspeficied"
                      :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; org mode
(use-package
 org
 :hook (org-mode . efs/org-mode-setup)
 :config
 (setq
  org-ellipsis " ▾"
  org-hide-emphasis-markers t
  org-confirm-babel-evaluate nil
  org-fontify-quote-and-verse-blocks t
  org-startup-folded 'content
  org-agenda-start-with-log-mode t
  org-log-done 'time
  org-log-into-drawer t)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)
    (clojure . t)
    (scheme . t)))
 (efs/org-font-setup))

;; custom bullets
(use-package
 org-bullets
 :after org
 :hook (org-mode . org-bullets-mode)
 :custom (org-bullets-bullet-list '("◉" "○" "✸" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq
   visual-fill-column-width 200
   visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package
 visual-fill-column
 :hook (org-mode . efs/org-mode-visual-fill))

(setq org-babel-clojure-backend 'cider)

;; org templates
(require 'org-tempo)

(add-to-list 'org-modules 'org-tempo t)
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
(add-to-list 'org-structure-template-alist '("pyt" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))

(use-package
 org-auto-tangle
 :defer t
 :hook (org-mode . org-auto-tangle-mode))

(use-package
 org-roam
 :ensure t
 :init (setq org-roam-v2-ack t)
 :custom
 (org-roam-directory "~/Notes/Roam/")
 (org-roam-completion-everywhere t)
 (org-roam-dailies-capture-templates
  '(("d" "default" entry "* %<%I:%M %p>: %?"
     :if-new
     (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
 :bind
 (("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  :map
  org-mode-map
  ("C-M-i" . completion-at-point)
  :map
  org-roam-dailies-map
  ("Y" . org-roam-dailies-capture-yesterday)
  ("T" . org-roam-dailies-capture-tomorrow))
 :bind-keymap ("C-c n d" . org-roam-dailies-map)
 :config
 (require 'org-roam-dailies) ;; Ensure the keymap is available
 (org-roam-db-autosync-mode))

;; Convenient key definitions
(use-package
 general
 :config
 (general-create-definer
  remacs/major-mode-leader-map
  :prefix "C-x")
 (general-create-definer remacs/ctrl-c-definer :prefix "C-c"))

;;; My functions
(defun remacs/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun remacs/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun remacs/kill-line ()
  "Kill the whole current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (kill-line)
  (kill-line))

(defun remacs/open-config ()
  "Open Emacs config file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun remacs/reload-config ()
  "Reload Emacs config file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun remacs/split-window-two ()
  "Split current window into two."
  (interactive)
  (split-window-right)
  (balance-windows))

(defun remacs/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

(defun remacs/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (let ((lsp-restart 'ignore))
    (delete-other-windows)
    (save-some-buffers)
    (let ((kill-buffer-query-functions '()))
      (mapc 'kill-buffer (buffer-list)))))

(defun remacs/text-scale-restore ()
  "Restore text scale."
  (interactive)
  (text-scale-set 0)
  (message "restored"))

(defun remacs/sha512 (&optional filename)
  "Compute sha512 message digest from FILENAME."
  (interactive)
  (let ((filename (or filename (read-file-name "Filename:"))))
    (secure-hash
     'sha512 (-> filename (message filename) (find-file-noselect)))))

(defun remacs/2base64 (&optional filename)
  "Encode FILENAME to base64."
  (-> filename (remacs/sha512) (base64-encode-string)))

(defun remacs/sha512-dir (dir)
  "Compute sha512 message digest to all files into DIR."
  (interactive)
  (mapcar
   (lambda (x)
     (cons (concat dir "/" x) (remacs/sha512 (concat dir "/" x))))
   (directory-files dir nil directory-files-no-dot-files-regexp)))

(defun remacs/load-darkmode ()
  "Load spacemacs darkmode."
  (interactive)
  (load-theme 'modus-vivendi-tinted t))

(defun remacs/load-lightmode ()
  "Load spacemacs lightmode."
  (interactive)
  (load-theme 'modus-operandi-deuteranopia t))

(defun remacs/load-dracula ()
  "Load doom dracula."
  (interactive)
  (load-theme 'doom-dracula t))

(defun remacs/choose-theme ()
  "Select color theme."
  (interactive)
  (let ((chose-theme
         (completing-read "Choose:" '(light dark dark-alternative))))
    (message chose-theme)
    (cond
     ((equal "dark" chose-theme)
      (remacs/load-darkmode))
     ((equal "light" chose-theme)
      (remacs/load-lightmode))
     ((equal "dark-alternative" chose-theme)
      (remacs/load-dracula))))
  (funcall major-mode))

(defun remacs/duplicate-line ()
  "Duplicate line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

;; from: https://emacs.stackexchange.com/a/34307
(defun remacs/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; from: https://emacs.stackexchange.com/a/34307
(defun remacs/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun remacs/switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun remacs/switch-to-message-buffer ()
  "Switch to messages buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun remacs/custom-tab-indent ()
  "Use tabs instead of spaces."
  (setq-local indent-tabs-mode 1))

(defun remacs/su-find-file (filename)
  "Find file with FILENAME and open a super user."
  (interactive "FFind file(sudo): ")
  (let ((file-to-open (concat "/sudo::" (expand-file-name filename))))
    (find-file file-to-open)))

(defun remacs/supress-warnings ()
  "Supress emergency messages."
  (interactive)
  (setq warning-minimum-level :emergency))

(defun remacs/default-warnings ()
  "Supress warning messages."
  (interactive)
  (setq warning-minimum-level :warning))

(defun remacs/my-change-number-at-point (change increment)
  "Change the number at point."
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match
         (number-to-string (funcall change number increment)))
        (goto-char point)))))

(defun remacs/my-increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a."
  (interactive "p")
  (remacs/my-change-number-at-point '+ (or increment 1)))

(defun remacs/my-decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x."
  (interactive "p")
  (remacs/my-change-number-at-point '- (or increment 1)))

(defun remacs/insert-comment ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (insert "//")
  (insert-char ?= 97))

(defun remacs/other-window-backward ()
  (interactive)
  (other-window -1))

;; Remap
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-i") #'remacs/other-window-backward)
(global-set-key (kbd "M-<down>") #'enlarge-window)
(global-set-key (kbd "M-<up>") #'shrink-window)
(global-set-key (kbd "M-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "C-c k") #'remacs/kill-line)
(global-set-key (kbd "C-c c") #'remacs/insert-comment)
(global-set-key [(control shift return)] #'remacs/smart-open-line-above)
(global-set-key [(shift return)] #'remacs/smart-open-line)

;; https://whhone.com/emacs-config/#modern-editor-behavior
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(keymap-global-unset "C-x <escape> <escape>") ; repeat-complex-command

(global-set-key
 (kbd "C-c a") #'remacs/my-increment-number-at-point)
(global-set-key
 (kbd "C-c x") #'remacs/my-decrement-number-at-point)
(global-set-key (kbd "M-s-<down>") #'remacs/move-line-down)
(global-set-key (kbd "M-s-<up>") #'remacs/move-line-up)

(remacs/ctrl-c-definer
  "2" '(remacs/split-window-two :which-key "split into two windows")
  "k" '(remacs/kill-current-buffer :which-key "kill current buffer")
  "s" '(remacs/switch-to-scratch-buffer :which-key "goto scratch buffer")
  "m" '(remacs/switch-to-message-buffer :which-key "goto message buffer")
  "l" '(remacs/kill-line :which-key "kill current line")
  "=" '(remacs/text-scale-restore :which-key "restore font size")
  "+" '(text-scale-increase :which-key "increase font size")
  "-" '(text-scale-decrease :which-key "decrease font size"))

(remacs/major-mode-leader-map
 "c"
 '(remacs/open-config :which-key "open emacs config"))
