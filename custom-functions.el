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

(defun gp/vscode-current-buffer-file-at-point1 ()
  (interactive)
  (start-process-shell-command "code"
                               nil
                               (concat "code --goto "
                                       (buffer-file-name)
                                       ":"
                                       (number-to-string (1+ (current-line))) ;; +1 who knows why
                                       ":"
                                       (number-to-string (current-column)))))

(defun gp/vscode-current-buffer-file-at-point ()
  (interactive)
  (start-process-shell-command "code"
                               nil
                               (concat "code --goto "
                                       (buffer-file-name)
                                       ":"
                                       (int-to-string (line-number-at-pos))
                                       ":"
                                       (int-to-string (1+ (current-column))))))

(define-key global-map (kbd "C-<escape>") 'gp/vscode-current-buffer-file-at-point)

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

(defun gp/goto-line ()
  (interactive)
  (let ((initial-state (and (boundp 'display-line-numbers-mode)
                            display-line-numbers-mode)))
    (display-line-numbers-mode)
    (unwind-protect
        (call-interactively 'goto-line)
      (when (not initial-state)
        (display-line-numbers-mode -1)))))

(define-key global-map (kbd "M-g M-g") 'gp/goto-line)

(defun gp/xref-directory-find-symbol-at-point ()
  (interactive)
  "Find all matches for symbol at point in the current directory."
  (require 'xref)
  (require 'grep)
  (let ((rgx (thing-at-point 'symbol t)))
    (when rgx
      (let ((files
             (project--files-in-directory default-directory
                                          nil)))
        (xref-show-xrefs
         (apply-partially #'project--find-regexp-in-files rgx files)
         nil)))))

(define-key global-map (kbd "C-c g") 'gp/xref-directory-find-symbol-at-point)
