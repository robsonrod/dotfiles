(defun robsonrod/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(, (car command-parts) nil 0 nil ,@ (cdr command-parts)))))

(defun robsonrod/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defvar robsonrod/polybar-process nil)
(defun robsonrod/kill-panel ()
  (interactive)
  (when robsonrod/polybar-process
    (ignore-errors
      (kill-process robsonrod/polybar-process)))
  (setq robsonrod/polybar-process nil))

(defun robsonrod/start-polybar ()
  (interactive)
  (robsonrod/kill-panel)
  (setq robsonrod/polybar-process (start-process-shell-command "polybar" nil "polybar main")))

(defun robsonrod/set-wallpaper ()
  (interactive)
  (start-process-shell-command "feh" nil "feh --bg-scale $HOME/.config/wallpaper/kraken.jpg"))

(defun robsonrod/exwm-init-hook ()
  (exwm-workspace-switch-create 0)
  (robsonrod/set-wallpaper)
  (robsonrod/start-polybar)
  (robsonrod/run-in-background "dunst")
  (robsonrod/run-in-background "picom")
  (robsonrod/run-in-background "low_bat_notifier"))

(use-package exwm
  :config
  (setq exwm-workspace-number 5)
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (pcase exwm-class-name
                ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title))))))
  (add-hook 'exwm-update-class-hook #'robsonrod/exwm-update-class)
  (add-hook 'exwm-init-hook #'robsonrod/exwm-init-hook)
    ;; Hide the modeline on all X windows
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line)))
  (start-process-shell-command "xrdb" nil "rxdb -merge ~/.Xresources")

  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output $MONITOR --primary --mode 3840x2400 --pos 0x0 --rotate normal --output DP-1-2 --off --output HDMI-2 --off --output HDMI-1 --off --output DP-1 --off --output DP-1-3 --off --output DP-2 --off --output DP-1-1 --off")
  (start-process-shell-command "xrandr" nil "xrandr --output $MONITOR --brightness 0.60")

  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)
  (setq exwm-workspace-warp-cursor t
        mouse-autoselect-window t
        focus-follows-mouse t)

  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j
          ?\C-\ ))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-r] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-t] . multi-vterm)
          ([?\s-c] . calc)
          ([?\s-f] . (lambda () (interactive)(start-process "" nil "firefox")))
          ([?\s-l] . (lambda () (interactive)(start-process-shell-command "screen_lock" nil "screen_lock")))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(provide 'init-exwm)
