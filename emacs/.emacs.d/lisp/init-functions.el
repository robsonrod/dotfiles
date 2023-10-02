(defun robsonrod/smart-open-line-above () 
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode." 
  (interactive) 
  (move-beginning-of-line nil) 
  (newline-and-indent) 
  (forward-line -1) 
  (indent-according-to-mode))

(defun robsonrod/smart-open-line () 
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode." 
  (interactive) 
  (move-end-of-line nil) 
  (newline-and-indent))

(defun robsonrod/open-my-config () 
  "Open my config file." 
  (interactive) 
  (find-file user-init-file))

(defun robsonrod/split-window-two () 
  "Split a window into two." 
  (interactive) 
  (split-window-right) 
  (balance-windows))

(defun robsonrod/kill-current-buffer () 
  "Kill the current buffer." 
  (interactive) 
  (kill-buffer nil))

(defun robsonrod/kill-all-buffers () 
  "Close all buffers." 
  (interactive) 
  (let ((lsp-restart 'ignore)) 
    (delete-other-windows) 
    (save-some-buffers) 
    (let ((kill-buffer-query-functions '())) 
      (mapc 'kill-buffer (buffer-list)))))

(defun robsonrod/text-scale-restore ()
  (interactive)
  (text-scale-set 0)
  (message "restored"))

(defun robsonrod/sha512 
    (&optional 
     filename)
  "Compute a sha512 message digest" 
  (interactive) 
  (let ((filename (or filename 
                      (read-file-name "Filename:")))) 
    (secure-hash 'sha512  (-> filename (message filename) 
                              (find-file-noselect)))))

(defun robsonrod/2base64 
    (&optional 
     filename)
  "Encode data to base64"
  (-> filename (robson/sha512) 
      (base64-encode-string)))

(defun robsonrod/sha512-dir (dir) 
  (interactive) 
  (mapcar (lambda (x)
            (cons (concat dir "/" x) (robson/sha512 (concat dir "/" x)))) 
          (directory-files dir nil directory-files-no-dot-files-regexp)))

(defun robsonrod/load-darkmode ()
  (interactive)
  (load-theme 'spacemacs-dark t))

(defun robsonrod/load-lightmode ()
  (interactive)
  (load-theme 'spacemacs-light t))

(defun robsonrod/load-dracula ()
  (interactive)
  (load-theme 'doom-dracula t))

(defun robsonrod/choose-theme()
  (interactive)
  (let ((chose-theme (ivy-read "Choose:" '(light dark dark-alternative))))
     (message chose-theme)
     (cond
      ((equal "dark" chose-theme) (robsonrod/load-darkmode))
      ((equal "light" chose-theme) (robsonrod/load-lightmode))
      ((equal "dark-alternative" chose-theme) (robsonrod/load-dracula))
      )
    )
  (funcall major-mode))



(provide 'init-functions)
