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

(define-key global-map (kbd "C-c ` c") 'gp/vscode-current-buffer-file)

(defun gp/day-theme ()
  (interactive)
  (disable-theme 'sanityinc-tomorrow-bright)
  (load-theme 'modus-operandi))

(defun gp/night-theme ()
  (interactive)
  (disable-theme 'modus-operandi)
  (load-theme 'sanityinc-tomorrow-bright))

(defun gp/find-init ()
  (interactive)
  (find-file (concat "/home/" (getenv "USER") "/.emacs.d/init.el" )))

;; custom functions to load keycodes
(defun gp/set-keycodes-filco ()
  (interactive)
  (shell-command "setxkbmap -keycodes evdev_custom_filco")
  (shell-command "xset r rate 200 60"))

(defun gp/set-keycodes-thinkpad_T460 ()
  (interactive)
  (shell-command "setxkbmap -keycodes evdev_custom_thinkpad_T460")
  (shell-command "xset r rate 200 60"))

(defun gp/set-keycodes-thinkpad_X200 ()
  (interactive)
  (shell-command "setxkbmap -keycodes evdev_custom_thinkpad_X200")
  (shell-command "xset r rate 200 60"))

(defun gp/set-key-rate ()
  (interactive)
  (shell-command "xset r rate 200 60"))
(gp/set-key-rate)
