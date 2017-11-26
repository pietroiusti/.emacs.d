;; TODO
;; create a key chord that behaves as Control
;; The same way C-[ behaves like ESC (which behaves like Meta)
;; Possible?


(setq user-full-name "Giulio Pietroiusti"
      user-mail-address "giulio.pietroiusti@gmail.com")

;; set default file encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; turn off beep
;;(setq ring-bell-function 'ignore)
;; get visual indication
(setq visible-bell 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'misc)

;; Reinclude tetris, which is not present in Fedora
(load "~/.emacs.d/tetris.el")

;; Don't show bars
(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

;; org agenda at startup
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)

;; show matching parenthesis
(show-paren-mode 1)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; enable auto-fill-mode when in text-mode and org-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; set tab to 4 spaces
(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; from the manual: If you want to make AUCTeX aware of style files and multi-file
;; documents right away, insert the following in your '.emacs' file. 
(setq TeX-auto-save t)
(setq Tex-parse-self t)
(setq-default TeX-master nil)


;; evil
(require 'evil)
;; emacs state as default in the following modes 
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'Info-mode 'emacs)
;; (evil-set-initial-state 'help-mode 'emacs)
;; (evil-set-initial-state 'eshell-mode 'emacs)
;; (evil-set-initial-state 'calendar-mode 'emacs)
;; (evil-set-initial-state 'erc-mode 'emacs)
;; (evil-set-initial-state 'Buffer-menu-mode 'emacs)
  
(setq evil-search-module 'evil-search)

;; change cursor color in different modes
;; https://github.com/bling/dotemacs/blob/master/config/init-evil.el
(setq evil-emacs-state-cursor '("grey" box))
(setq evil-motion-state-cursor '("red" box))
(setq evil-normal-state-cursor '("red" box))
(setq evil-visual-state-cursor '("red" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))
;; TODO: change color of cursor when it is in the minibuffer

;; emacs keybindings as a default!
(setq evil-default-state 'emacs
      evil-insert-state-modes nil
      evil-motion-state-modes nil)

(evil-mode 1)


(require 'helm-config)
(helm-mode 1)

;; org
(require 'org)
(setq org-startup-indented t)
(setq org-indent-mode t)
(setq org-hide-leading-stars t)
(setq org-src-fontify-natively t)
;; The four Org commands org-store-link, org-capture, org-agenda, and
;; org-iswitchb should be accessible through global keys (i.e.,
;; anywhere in Emacs, not just in Org buffers).
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; If you enable Delete Selection mode, a minor mode, then inserting
;; text while the mark is active causes the selected text to be
;; deleted first. This also deactivates the mark.
(delete-selection-mode 1)

;; simulate vim 'f' (also backward)
(global-set-key (kbd "C-c f") 'iy-go-up-to-char)
(global-set-key (kbd "C-c F") 'iy-go-up-to-char-backward)
;; make the every key behave normally after these commands
(setq iy-go-to-char-override-local-map 'nil)
 

;; avy
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-line)


(require 'js2-mode)
;; js2-mode as a defalut for js files
(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))
;;TODO
;;autocompletion


;;magit
(global-set-key (kbd "C-x g") 'magit-status)


;; sublimity  (call command M-x sublimity-mode)
(require 'sublimity)
;; (require 'sublimity-scroll)
(require 'sublimity-map) ;; experimental
;; (require 'sublimity-attractive)

(setq sublimity-map-size 20)
(setq sublimity-map-fraction 0.3)
(setq sublimity-map-text-scale -7)

(add-hook 'sublimity-map-setup-hook
(lambda ()
(setq buffer-face-mode-face '(:family "Monospace"))
(buffer-face-mode)))

(sublimity-map-set-delay nil)


;; solarized-emacs
;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

;; (load-theme 'solarized-light t)
(load-theme 'solarized-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/meetings.org" "~/Dropbox/org/todos.org" "~/Dropbox/org/activities.org")))
 '(package-selected-packages
   (quote
    (avy sublimity solarized-theme magit js2-mode iy-go-to-char helm expand-region evil auctex ace-jump-mode)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
