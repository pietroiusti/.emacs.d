;; TODO
;; create a key chord that behaves as Control
;; The same way C-[ behaves like ESC (which behaves like Meta)
;; Possible?

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
;; (setq org-startup-indented t)
;; (setq org-indent-mode t)
;; (setq org-hide-leading-stars t)
(setq org-src-fontify-natively t)

;; If you enable Delete Selection mode, a minor mode, then inserting
;; text while the mark is active causes the selected text to be
;; deleted first. This also deactivates the mark.
(delete-selection-mode 1)

;; simulate vim 'f' (also backward)
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
;; make the every key behave normally after these commands
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


(require 'js2-mode)
;; js2-mode as a defalut for js files
(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))
;;TODO
;;autocompletion


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
 '(package-selected-packages
   (quote
    (js2-mode zenburn-theme web-mode sublimity speed-type solarized-theme pomidor php-mode org-ref org-bullets magit iy-go-to-char impatient-mode expand-region evil company-tern auto-complete auctex ag ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
