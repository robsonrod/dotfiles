;;; init.el --- my emacs file -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;; My Emacs file

;;; Code:

(message "Starting emacs")
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
      scroll-step 1                 ;
      dictionary-server "dict.org"
      vc-follow-symlinks t)
                                        ; scroll 1 line with keyboard
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) backup-by-copying t ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old
;; window title
(setq frame-title-format "%b - emacs")

;; window resize
(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha . (98 . 98)))
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
                              dired-mode-hook pdf-view-mode-hook)) 
  (add-hook mode (lambda () 
                   (display-line-numbers-mode -1))))

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; font configuration
(set-face-attribute 'default nil 
                    :font "JetBrainsMono Nerd Font" 
                    :height 100)
(set-face-attribute 'fixed-pitch nil 
                    :font "RobotoMono Nerd Font" 
                    :height 100)
(set-face-attribute 'variable-pitch nil 
                    :font "Iosevka Nerd Font" 
                    :height 100)

;; yes or no question
(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom functions
(global-set-key [(control shift return)] 'robsonrod/smart-open-line-above)
(global-set-key [(shift return)] 'robsonrod/smart-open-line)

;; custom window management
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)

(defun robsonrod/modeline-contitional-buffer-encoding () 
  "Hide \"LF UTF-8\" in modeline.
   It is expected of files to be encoded with LF UTF-8, so only show
   the encoding in the modeline if the encoding is worth notifying
   the user."
  (setq-local doom-modeline-buffer-encoding (unless (and (memq (plist-get (coding-system-plist
                                                                           buffer-file-coding-system) 
                                                                          :category) 
                                                               '(coding-category-undecided
                                                                 coding-category-utf-8)) 
                                                         (not (memq (coding-system-eol-type
                                                                     buffer-file-coding-system) 
                                                                    '(1 2)))) t)))

(add-hook 'after-change-major-mode-hook #'robsonrod/modeline-contitional-buffer-encoding)
(add-hook 'makefile-mode-hook #'robsonrod/custom-tab-indent)
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

(server-start)

;; 
(use-package general
  :config
  (general-create-definer robsonrod/major-mode-leader-map
    :prefix "C-,")
  (general-create-definer robsonrod/ctrl-c-definer
    :prefix "C-c"))

(global-set-key (kbd "M-o") 'ff-find-related-file)

(defvar robsonrod/exwm-running
  (cond ((and (memq window-system '(x))
              (seq-contains-p command-line-args "--use-exwm")
              :true))
        (t :false)))

(when (eq robsonrod/exwm-running :true)
  (message "Starting EXWM")
  (require 'init-exwm))

(require 'init-gui)
(require 'init-functions)
(require 'init-eshell)
(require 'init-lsp)
(require 'init-dired)
(require 'init-company)
(require 'init-org)
(require 'init-magit)
(require 'init-vterm)
(require 'init-templates)
(require 'init-cpp)
(require 'init-rust)
(require 'init-elisp)
(require 'init-pdf)
(require 'init-completion)
(require 'init-project)
(require 'init-clojure)
(require 'init-scheme)
(require 'init-dap)
(require 'init-prog-common)
(require 'init-treemacs)
(require 'init-sh)
(require 'init-emacs-misc)
(require 'init-commenter)
(require 'init-rss)
(require 'init-dictionary)
(require 'init-undo)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eldoc-documentation-functions nil t nil "Customized with use-package lsp-mode")
 '(package-selected-packages
   '(all-the-icons-ivy-rich all-the-icons-ivy ivy-posframe ivy-prescient
                            wgrep flx yaml-mode which-key
                            visual-fill-column undo-fu-session undo-fu
                            treemacs-all-the-icons tempel-collection
                            spacemacs-theme shfmt rust-mode ripgrep
                            rainbow-delimiters perspective pdf-tools
                            paredit org-roam org-bullets
                            org-auto-tangle multi-vterm
                            modern-cpp-font-lock minions magit lsp-ui
                            lsp-ivy ivy-rich ivy-hydra iedit helpful
                            git-gutter-fringe general geiser-mit
                            flycheck-clj-kondo fish-mode
                            fish-completion find-file-in-project exwm
                            exec-path-from-shell evil-nerd-commenter
                            eshell-syntax-highlighting
                            eshell-prompt-extras elisp-format
                            elfeed-org doom-themes doom-modeline
                            dockerfile-mode dired-single dired-ranger
                            dired-rainbow dired-open
                            dired-hide-dotfiles dired-collapse
                            diminish desktop-environment dap-mode
                            counsel-projectile corfu
                            company-restclient company-box
                            clang-format cider ccls
                            auto-package-update auto-compile
                            all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
