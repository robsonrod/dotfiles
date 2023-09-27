(defun robson/sha512 
    (&optional 
     filename)
  "Compute a sha512 message digest" 
  (interactive) 
  (let ((filename (or filename 
                      (read-file-name "Filename:")))) 
    (secure-hash 'sha512  (-> filename (message filename) 
                              (find-file-noselect)))))

(defun robson/2base64 
    (&optional 
     filename)
  "Encode data to base64"
  (-> filename (robson/sha512) 
      (base64-encode-string)))


(defun robson/sha512_dir (dir) 
  (interactive) 
  (mapcar (lambda (x)
            (cons (concat dir "/" x) (robson/sha512 (concat dir "/" x)))) 
          (directory-files dir nil directory-files-no-dot-files-regexp)))

(provide init-functions)
