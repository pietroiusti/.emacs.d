(setq user-full-name "Giulio Pietroiusti"
      user-mail-address "giulio.pietroiusti@gmail.com")

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
;; emacs keybindings as a default!
(setq evil-default-state 'emacs
      evil-insert-state-modes nil
      evil-motion-state-modes nil)
(evil-mode 1)
 

(require 'helm-config)
(helm-mode 1)

;; org
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; If you enable Delete Selection mode, a minor mode, then inserting
;; text while the mark is active causes the selected text to be
;; deleted first. This also deactivates the mark.
(delete-selection-mode 1)

;; simulate vim 'f' (also backward)
;;
;; (key-chord-mode 1)
;; (key-chord-define-global "kk" 'iy-go-to-char)
;; (key-chord-define-global "jj" 'iy-go-to-char-backward)
;; without key-chord:
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
;; make the every key behave normally after those commands
(setq iy-go-to-char-override-local-map 'nil)


;;
;; ace jump mode major function
;; 
(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-;") 'ace-jump-mode)

;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


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
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (yasnippet zenburn-theme web-mode speed-type solarized-theme pomidor org-ref org-bullets magit js2-mode iy-go-to-char impatient-mode expand-region evil auto-complete auctex ace-jump-mode)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
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
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
