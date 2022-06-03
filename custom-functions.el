(defun gp/kill-buffer-open-directory ()
  "If the buffer is visiting a file, kill the buffer and open the
directory the file is in."
  (interactive)
  (let ((cb (current-buffer)))
    (cond
     ((buffer-file-name)
      (find-file (file-name-directory (buffer-file-name)))
      (kill-buffer cb))
     (t
      (message "Buffer is visiting no file.")))))
