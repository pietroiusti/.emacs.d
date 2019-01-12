(use-package exwm
    :ensure t
    :config
    (require 'exwm)
    (require 'exwm-config)

    (defun exwm-config-myconf ()
      "My configuration of EXWM."
      ;; Set the initial workspace number.
      (setq exwm-workspace-number 4)
      ;; Make class name the buffer name
      (add-hook 'exwm-update-class-hook
                (lambda ()
                  (exwm-workspace-rename-buffer exwm-class-name)))
      ;; 's-r': Reset
      (exwm-input-set-key (kbd "s-r") #'exwm-reset)
      ;; 's-w': Switch workspace
      (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
      ;; 's-N': Switch to certain workspace
      (dotimes (i 10)
        (exwm-input-set-key (kbd (format "s-%d" i))
                            `(lambda ()
                               (interactive)
                               (exwm-workspace-switch-create ,i))))
      ;; 's-&': Launch application
      (exwm-input-set-key (kbd "s-&")
                          (lambda (command)
                            (interactive (list (read-shell-command "$ ")))
                            (start-process-shell-command command nil command)))
      ;; Line-editing shortcuts
      (setq exwm-input-simulation-keys
            '(([?\C-b] . [left])
              ([?\C-f] . [right])
              ([?\C-p] . [up])
              ([?\C-n] . [down])
              ([?\C-a] . [home])
              ([?\C-e] . [end])
              ([?\M-v] . [prior])
              ([?\C-v] . [next])
              ([?\C-d] . [delete])
              ([?\C-k] . [S-end delete])))

      ;; window moving
      (exwm-input-set-key (kbd "s-h") #'windmove-left)
      (exwm-input-set-key (kbd "s-b") #'windmove-left)
      (exwm-input-set-key (kbd "s-j") #'windmove-down)
      (exwm-input-set-key (kbd "s-n") #'windmove-down)
      (exwm-input-set-key (kbd "s-k") #'windmove-up)
      (exwm-input-set-key (kbd "s-p") #'windmove-up)
      (exwm-input-set-key (kbd "s-l") #'windmove-right)
      (exwm-input-set-key (kbd "s-f") #'windmove-right)

      ;; window swapping
      (defun ambrevar/swap-windows (&optional w1 w2)
        "If 2 windows are up, swap them.
      Else if W1 is a window, swap it with current window.
      If W2 is a window too, swap both."
        (interactive)
        (unless (or (= 2 (count-windows))
                    (windowp w1)
                    (windowp w2))
          (error "Ambiguous window selection"))
        (let* ((w1 (or w1 (car (window-list))))
               (w2 (or w2
                       (if (eq w1 (car (window-list)))
                           (nth 1 (window-list))
                         (car (window-list)))))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (with-temp-buffer
            ;; Some buffers like EXWM buffers can only be in one live buffer at once.
            ;; Switch to a dummy buffer in w2 so that we don't display any buffer twice.
            (set-window-buffer w2 (current-buffer))
            (set-window-buffer w1 b2)
            (set-window-buffer w2 b1))
          (set-window-start w1 s2)
          (set-window-start w2 s1))
        (select-window w1))
      (global-set-key (kbd "C-x \\") 'swap-windows)

      (defun ambrevar/swap-windows-left ()
        "Swap current window with the window to the left."
        (interactive)
        (ambrevar/swap-windows (window-in-direction 'left)))
      (defun ambrevar/swap-windows-below ()
        "Swap current window with the window below."
        (interactive)
        (ambrevar/swap-windows (window-in-direction 'below)))
      (defun ambrevar/swap-windows-above ()
        "Swap current window with the window above."
        (interactive)
        (ambrevar/swap-windows (window-in-direction 'above)))
      (defun ambrevar/swap-windows-right ()
        "Swap current window with the window to the right."
        (interactive)
        (ambrevar/swap-windows (window-in-direction 'right)))

      (exwm-input-set-key (kbd "s-\\") 'ambrevar/toggle-window-split)
      (exwm-input-set-key (kbd "s-H") 'ambrevar/swap-windows-left)
      (exwm-input-set-key (kbd "s-B") 'ambrevar/swap-windows-left)
      (exwm-input-set-key (kbd "s-J") 'ambrevar/swap-windows-below)
      (exwm-input-set-key (kbd "s-N") 'ambrevar/swap-windows-below)
      (exwm-input-set-key (kbd "s-K") 'ambrevar/swap-windows-above)
      (exwm-input-set-key (kbd "s-P") 'ambrevar/swap-windows-above)
      (exwm-input-set-key (kbd "s-L") 'ambrevar/swap-windows-right)
      (exwm-input-set-key (kbd "s-F") 'ambrevar/swap-windows-right)

      ;; show X windows from any workspace
      (setq exwm-workspace-show-all-buffers t)
      (setq exwm-layout-show-all-buffers t)

      ;; include exwm-title in buffer name
      (defun exwm-rename-buffer ()
        (interactive)
        (exwm-workspace-rename-buffer
         (concat exwm-class-name ":"
                 (if (<= (length exwm-title) 50) exwm-title
                   (concat (substring exwm-title 0 49) "...")))))

      (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
      (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

      ; Multiple monitors
      (require 'exwm-randr)
      (setq exwm-randr-workspace-output-plist '(1 "HDMI2" 2 "HDMI2"
      3 "HDMI2" 4 "HDMI2" 5 "HDMI2" 6 "HDMI2" 7 "HDMI2" 8 "HDMI2"
      9 "HDMI2"))
      (add-hook 'exwm-randr-screen-change-hook
                (lambda ()
                  (start-process-shell-command
                   "xrandr" nil "xrandr --output HDMI2 --above eDP1 --auto")))
      (exwm-randr-enable)

      ;; system tray
      (require 'exwm-systemtray)
      (exwm-systemtray-enable)


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
      (exwm-input-set-key (kbd "s-S")
                          (lambda ()
                            (interactive)
                            (shell-command "i3lock -c 000000 ; systemctl suspend")))

      ;; launch emacsclient
      (exwm-input-set-key (kbd "s-e")
                          (lambda ()
                            (interactive)
                            (start-process-shell-command "emacsclient" nil "emacsclient -c")))

      ;; TODO quit window with s-Q

      ;; start compton
      (start-process-shell-command "compton" nil "compton -b")

      ;; start nextcloud
      ;; (shell-command "nextcloud")

      ;;nm-applet
      ;; (shell-command "nm-applet")

      ;; set keycodes
      (start-process-shell-command "setxkbmap" nil "setxkbmap -keycodes evdev_custom_thinkpad_X200")

      ;; disable touchpad TODO

      ;; set background TODO

      (symon-mode)

      ;; Enable EXWM
      (exwm-enable)
      ;; Configure Ido
      (exwm-config-ido)
      ;; Other configurations
      (exwm-config-misc))

    (exwm-config-myconf))
