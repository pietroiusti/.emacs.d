(use-package exwm
  :ensure t
  :config

  ;; allow resizing X windows by dragging their right edge
  (setq window-divider-default-right-width 1)
  (window-divider-mode)

  ;; Load EXWM.
  (require 'exwm)

  ;; Set the initial number of workspaces (they can also be created later).
  (setq exwm-workspace-number 10)

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
					;    all windows are probably the same.  Using window titles for them makes
  ;;   more sense.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
		(exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
			(string-prefix-p "sun-awt-X11-" exwm-instance-name)
			(string= "gimp" exwm-instance-name))
		(exwm-workspace-rename-buffer exwm-title))))

  ;; Global keybindings can be defined with `exwm-input-global-keys'.
  (setq exwm-input-global-keys
	`(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
			(lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))

	  ;; window selection keybindings
	  ([?\s-h] . windmove-left)
	  ([?\s-b] . windmove-left)
	  ([s-left] . windmove-left) ; with arrow keys somehow with don't have to use the ?\ sintax
	  ([?\s-j] . windmove-down)
	  ([?\s-n] . windmove-down)
	  ([s-down] . windmove-down)
	  ([?\s-k] . windmove-up)
	  ([?\s-p] . windmove-up)
	  ([s-up] . windmove-up)
	  ([?\s-l] . windmove-right)
	  ([?\s-f] . windmove-right)
	  ([s-right] . windmove-right)

	  ;; swap buffers keybindings
	  ([?\s-H] . buf-move-left)
	  ([?\s-B] . buf-move-left)
	  ([S-s-left] . buf-move-left)
	  ([?\s-J] . buf-move-down)
	  ([?\s-N] . buf-move-down)
	  ([S-s-down] . buf-move-down)
	  ([?\s-K] . buf-move-up)
	  ([?\s-P] . buf-move-up)
	  ([S-s-up] . buf-move-up)
	  ([?\s-L] . buf-move-right)
	  ([?\s-F] . buf-move-right)
	  ([S-s-right] . buf-move-right)

	  ([s-return] . (lambda (command)
			  (interactive (list (read-shell-command "$ ")))
			  (start-process-shell-command command nil command)))

          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
		      (interactive)
		      (start-process "" nil "/usr/bin/slock")))))


  ;; To add a key binding only available in line-mode, simply define it
  ;; in `exwm-mode-map'.  The following shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;;  The value of `exwm-input-simulation-keys` is a list of cons cells
  ;; (SRC . DEST), where SRC is the key sequence you press and DEST is
  ;; what EXWM actually sends to application.  Note that both SRC and
  ;; DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
	'(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
	  ([?\M-d] . [C-delete])
	  ([M-backspace] . [C-backspace])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [f3])
	  ([?\C-r] . [S-f3])
	  ;; newline
	  ([?\C-j] . [return])
	  ([?\C-o] . [return left])
	  ;; undo
	  ([?\C-/] . [C-z])
	  ;; cancel
	  ([?\C-g] . [escape])))

  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (when (and exwm-class-name
			 (string= exwm-class-name "st-256color"))
		(exwm-input-set-local-simulation-keys nil))))

  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (when (and exwm-class-name
			 (string= exwm-class-name "Xfce4-terminal"))
		(exwm-input-set-local-simulation-keys nil))))

  ;; see x windows from anywhere
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  ;; name exwm buffers with exwm-titles
  (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

  ;; Customize ediff to open the *Ediff Control Panel* in a new window
  ;; instead of a new frame.
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; system tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Hide mode-line by default
  ;;(add-hook 'exwm-manage-finish-hook #'exwm-layout-hide-mode-line)

  ;; Autohide minibuffer & echo area (might be buggish...)
  ;;(setq exwm-workspace-minibuffer-position 'bottom)

  ;; Start some processes
  ;; (start-process "" nil "nextcloud")
  (start-process "" nil "picom" "-b")
  (start-process "" nil "nm-applet" "--no-agent")
  (start-process-shell-command "" nil "feh --bg-scale ~/Pictures/levitating-gnu.png")

  ;; Custom keybindings
  ;;
  ;; screen brightness
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
		      (lambda ()
			(interactive) (start-process-shell-command "xbacklight" nil "xbacklight -inc 5")))
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
		      (lambda ()
			(interactive) (start-process-shell-command "xbacklight" nil "xbacklight -dec 5")))
  ;; volume
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
		      (lambda ()
			(interactive)
			(start-process-shell-command "amixer" nil "amixer set Master 3%+")))
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
		      (lambda ()
			(interactive)
			(start-process-shell-command "amixer" nil "amixer set Master 3%-")))
  (exwm-input-set-key (kbd "<XF86AudioMute>")
		      (lambda ()
			(interactive)
			(start-process-shell-command "amixer" nil "amixer set Master toggle")))
  ;; suspend
  (exwm-input-set-key (kbd "s-s")
		      (lambda ()
			(interactive)
			(shell-command "systemctl suspend")))
  ;; lock and suspend
  (exwm-input-set-key (kbd "s-S")
		      (lambda ()
			(interactive)
			(shell-command "i3lock -c 000000 ; systemctl suspend")))

  ;; chromium
  (exwm-input-set-key (kbd "s-c")
		      (lambda ()
			(interactive)
			(start-process-shell-command "chromium" nil "chromium")))

  (exwm-input-set-key (kbd "s-C")
		      (lambda ()
			(interactive)
			(start-process-shell-command "chromium" nil "chromium --incognito")))

  ;; emacsclient
  (exwm-input-set-key (kbd "s-e")
		      (lambda ()
			(interactive)
			(start-process-shell-command "emacsclient" nil "emacsclient -c")))

  (exwm-input-set-key (kbd "s-m")
		      (lambda ()
			(interactive)
			(exwm-workspace-toggle-minibuffer)))

  (require 'exwm-randr)
  ;; (setq exwm-randr-workspace-output-plist '(0 "DP1" 1 "DP1" 2 "DP1"
  ;;  					    3 "DP1" 4 "DP1"
  ;;  					    5 "DP1" 6 "DP1"
  ;;  					    7 "DP1" 8 "DP1"))

  ;; (setq exwm-randr-workspace-output-plist '(1 "HDMI2" 2 "HDMI2"
  ;; 					    3 "HDMI2" 4 "HDMI2" 5 "HDMI2" 6 "HDMI2" 7 "HDMI2" 8 "HDMI2"
  ;; 					    9 "HDMI2"))
  ;; (add-hook 'exwm-randr-screen-change-hook
  ;;           (lambda ()
  ;;             (start-process-shell-command
  ;;              "xrandr" nil "xrandr --output HDMI2 --left-of eDP1 --auto")))
  (exwm-randr-enable)

  ;; Do not forget to enable EXWM. It will start by itself when things are
  ;; ready.  You can put it _anywhere_ in your configuration.
  (exwm-enable))
