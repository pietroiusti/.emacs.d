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

(define-key global-map (kbd "C-c q") 'gp/kill-buffer-open-directory)

;; (defun gp/change-parentheses (char)
;;   (interactive "cNew Parentheses character: ")
;;   (cond
;;    ( (or (char-equal (preceding-char) ?\()
;;          (char-equal (following-char) ?\)))
;;      (forward-sexp ... 

(defun gp/gvim-current-buffer-file ()
  (interactive)
  (start-process "gvim" nil "gvim" (buffer-file-name)))

(define-key global-map (kbd "C-c v") 'gp/gvim-current-buffer-file)

(defun gp/vscode-current-buffer-file ()
  (interactive)
  (start-process "code" nil "code" (buffer-file-name)))

(define-key global-map (kbd "C-c ~ c") 'gp/vscode-current-buffer-file)
