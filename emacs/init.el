;; The default is 800 kilobytes.  Measured in bytes.

;; ********** General config editor
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

;; Thanks, but no thanks
(setq inhibit-startup-message t)

;; Enababling line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook eshell-mode-hook)) 
  (add-hook mode (lambda () 
		   (display-line-numbers-mode 0))))

(global-hl-line-mode +1)
(scroll-bar-mode -1)			; Disable visible scrollbar
(tool-bar-mode -1)			; Disable the toolbar
(tooltip-mode -1)			; Disable tooltips
(set-fringe-mode 10)			; Give some breathing room
(menu-bar-mode -1)			; Disable the menu bar
(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil 
		    :font "Fira Code" 
		    :height 72)

(setq-default cursor-type 'bar)

;; ESC Cancels alls
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Changing yes or no question
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defun kill-other-buffers () 
  "Kill all other buffers." 
  (interactive) 
  (mapc 'kill-buffer (delq (current-buffer) 
			   (remove-if-not 'buffer-file-name (buffer-list)))))

(global-set-key (kbd "C-x k") 'kill-other-buffers)

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

;; Theme
(use-package 
  doom-themes 
  :ensure t 
  :config (setq doom-themes-enable-bold t doom0themes-enable-italic t) 
  (load-theme 'doom-dracula t))

;; Rainbow delimiters
(use-package 
  rainbow-delimiters 
  :ensure t 
  :hook (prog-mode . rainbow-delimiters-mode))

;; Key helper
(use-package 
  which-key 
  :init (which-key-mode) 
  :diminish which-key-mode 
  :config (setq which-key-idle-delay 0.3))

;; Switch buffer using counsel
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(global-set-key (kbd "C-c r") 'counsel-rg)
(global-set-key (kbd "C-c z") 'counsel-fzf)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

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
	 :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history)) 
  :config (setq ivy-initial-inputs-alias nil) 
  (setq counsel-find-file-ignore-regexp ".*cache"))

(use-package 
  ivy-rich
  :after ivy counsel
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

;; Hook to modes
(defun dw/evil-hook () 
  (dolist (mode '(custom-mode eshell-mode git-rebase-mode erc-mode circe-server-mode circe-chat-mode
			      circe-query-mode sauron-mode term-mode)) 
    (add-to-list 'evil-emacs-state-modes mode)))

(global-set-key (kbd "C-M-u") 'universal-argument)

;; Better experience with evil
(use-package 
  undo-tree 
  :init (global-undo-tree-mode 1) 
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Watch out with arrow keys
(defun dw/dont-arrow-me-bro () 
  (interactive) 
  (message "Arrow keys are bad, you know?"))

(use-package 
  general 
  :config (general-evil-setup t) 
  (general-create-definer rar/leader-key-def 
    :keymaps '(normal insert visual emacs) 
    :prefix "C-;" 
    :non-normal-prefix "C-;" 
    :global-prefix "C-;") 
  (general-create-definer ctrl-c-keys 
    :prefix "C-c"))

(rar/leader-key-def "t" 
  '(:ignore t 
	    :which-key "toggles")
  "tw" 'whitespace-mode "tt" '(counsel-load-theme :which-key "choose theme") "lf"
  'elisp-format-buffer)

(ctrl-c-keys "c" 
  '(:ignore t 
	    :which-key "chat")
  "cb" 'erc-switch-to-buffer "cc" 'dw/connect-irc "ca" 'erc-track-switch-buffer)

;; Lets be evil
(use-package 
  evil 
  :init (setq evil-want-integration t) 
  (setq evil-want-keybinding nil) 
  (setq evil-want-C-u-scroll t) 
  (setq evil-want-C-i-jump nil) 
  (setq evil-respect-visual-line-mode t) 
  (setq evil-undo-system 'undo-tree) 
  :config (add-hook 'evil-mode-hook 'dw/evil-hook) 
  (evil-mode 1) 
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) 
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) 
  (define-key evil-insert-state-map (kbd "C-s") 'evil-write)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line) 
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro) 
  (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro) 
  (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro) 
  (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro) 
  (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro) 
  (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro) 
  (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro) 
  (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro) 
  (evil-set-initial-state 'messages-buffer-mode 'normal) 
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package 
  evil-collection 
  :after evil 
  :init (setq evil-collection-company-use-tng nil) ;; Is this a bug in evil-collection?
  :custom (evil-collection-outline-bind-tab-p nil) 
  :config (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list)) 
  (evil-collection-init))

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

(rar/leader-key-def "ts" '(hydra-text-scale/body :which-key "scale text"))

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

(rar/leader-key-def "pf"  'counsel-projectile-find-file "ps"  'counsel-projectile-switch-project
  "pF"  'counsel-projectile-rg
					;"pF"  'consult-ripgrep
  "pp"  'counsel-projectile "pc"  'projectile-compile-project "pd"  'projectile-dired)

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

(rar/leader-key-def "g" 
  '(:ignore t 
	    :which-key "git")
  "gs"  'magit-status "gd"  'magit-diff-unstaged "gc"  'magit-branch-or-checkout "gl" 
  '(:ignore t 
	    :which-key "log")
  "glc" 'magit-log-current "glf" 'magit-log-buffer-file "gb"  'magit-branch "gP" 'magit-push-current
  "gp"  'magit-pull-branch "gf"  'magit-fetch "gF"  'magit-fetch-all "gr" 'magit-rebase)

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

(rar/leader-key-def "l" 
  '(:ignore t 
	    :which-key "lsp")
  "ld" 'xref-find-definitions "lr" 'xref-find-references "ln" 'lsp-ui-find-next-reference "lp"
  'lsp-ui-find-prev-reference "ls" 'counsel-imenu "le" 'lsp-ui-flycheck-list "lS"
  'lsp-ui-sideline-mode "lX" 'lsp-execute-code-action)

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
  yasnippet 
  :hook (prog-mode . yas-minor-mode) 
  :config (yas-reload-all))

;;; Build system
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

(rar/leader-key-def "mk" '(hydra-build/body :which-key "compile commands"))

;; Go Lang
(use-package 
  go-mode 
  :hook (go-mode . lsp-deferred))


;; Lisp
(use-package 
  lispy 
  :hook ((emacs-lisp-mode . lispy-mode) 
	 (scheme-mode . lispy-mode)))

(use-package 
  lispyville 
  :hook ((lispy-mode . lispyville-mode)) 
  :config (lispyville-set-key-theme '(operators c-w additional additional-movement slurp/barf-cp
						prettify)))

(rar/leader-key-def "e" 
  '(:ignore t 
	    :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(rar/leader-key-def :keymaps '(visual) 
  "er" '(eval-region :which-key "eval region"))

(use-package 
  cider 
  :mode "\\.clj[sc]?\\'" 
  :config (evil-collection-cider-setup))

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
			       "https://www.reddit.com/r/Clojure/.rss") elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
:bind ("C-x w" . elfeed))

(rar/leader-key-def "f" 
  '(:ignore t 
	    :which-key "feed")
  "fi"  '(elfeed :which-key "elfeed init") "fa"  '(elfeed-update :which-key "elfeed init"))

;; Nerd commenter
(use-package 
  evil-nerd-commenter 
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package 
  all-the-icons-dired)
(use-package 
  dired 
  :ensure nil 
  :defer 1 
  :commands (dired dired-jump) 
  :config (setq dired-listing-switches "-agho --group-directories-first" dired-omit-files
		"^\\.[^.].*" dired-omit-verbose nil dired-hide-details-hide-symlink-targets nil
		delete-by-moving-to-trash t) 
  (autoload 'dired-omit-mode "dired-x") 
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
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")) 
  (use-package 
    dired-single 
    :defer t) 
  (use-package 
    dired-ranger 
    :defer t) 
  (use-package 
    dired-collapse 
    :defer t) 
  (evil-collection-define-key 'normal 'dired-mode-map "h" 'dired-single-up-directory "H"
    'dired-omit-mode "l" 'dired-single-buffer "y" 'dired-ranger-copy "X" 'dired-ranger-move "p"
    'dired-ranger-paste))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history) 
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol) 
  (evil-normalize-keymaps) 
  (setq eshell-history-size         10000 eshell-buffer-maximum-lines 10000 eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package 
  eshell-git-prompt)

(use-package 
  eshell 
  :hook (eshell-first-time-mode . efs/configure-eshell) 
  :config (with-eval-after-load 'esh-opt 
	    (setq eshell-destroy-buffer-when-process-dies t) 
	    (setq eshell-visual-commands '("htop" "zsh" "vim"))) 
  (eshell-git-prompt-use-theme 'powerline))
