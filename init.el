(setq user-full-name "Giulio Pietroiusti"
      user-mail-address "giulio.pietroiusti@gmail.com")

;; Reinclude tetris, which is not present in Fedora
(load "~/.emacs.d/tetris.el")

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; set default file encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; turn off beep
;;(setq ring-bell-function 'ignore)
;; get visual indication
(setq visible-bell 1)

;; Don't show bars
(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

;; show matching parenthesis
(show-paren-mode 1)

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
;; org capture
(setq org-default-notes-file "~/Dropbox/org/capture.org")

;; org agenda at startup
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)

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
 '(org-agenda-files (quote ("~/Dropbox/org/todos.org")))
 '(package-selected-packages
   (quote
    (solarized-theme ox-twbs org-edna js2-mode iy-go-to-char helm evil avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
