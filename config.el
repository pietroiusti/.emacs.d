(setq user-full-name "Giulio Pietroiusti"
      user-mail-address "giulio.pietroiusti@gmail.com")

(load "~/.emacs.d/tetris.el")

(require 'cl)

(pdf-tools-install)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)

(require 'paredit)
(require 'rainbow-delimiters)
(require 'company)
;; Enable paredit, rainbow-delimiters and show-paren-mode for Emacs lisp
;; mode (mode to edit Emacs files *.el) and lisp-interaction-mode (mode
;; to edit *scratch* buffer)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode t)
            (rainbow-delimiters-mode t)
            (show-paren-mode 1)
            ))
(add-hook 'lisp-interaction-mode
          (lambda ()
            (paredit-mode t)
            (rainbow-delimiters-mode t)
            (show-paren-mode 1)
            ))

;; C style
(setq c-default-style "linux"
      c-basic-offset 4)

;; set default file encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; get visual indication
(setq visible-bell 1)

(when window-system (global-hl-line-mode t))

;; Font font size 
(set-face-attribute 'default (selected-frame) :height 110)

;; Don't show tool bar
(tool-bar-mode 0)
;; Don't show menu bar
(menu-bar-mode 0)
;; Don't show scroll bar
(when window-system
  (scroll-bar-mode 0))

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

;; Change cursor color in different modes
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

;; Ido mode and ido vertical mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(global-set-key (kbd "M-x") 'smex)

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


;; Define todo states
(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "DONE" )))

;; Show org agenda and my /org folder at startup
(setq inhibit-splash-screen t)
(org-agenda-list)
(switch-to-buffer "*Org Agenda*")
(delete-other-windows)

(split-window-right)
(other-window 1)
(find-file "~/Dropbox/org/")
(beginning-of-buffer)
(other-window 1)

;; refresh org agenda
(add-hook 'after-init-hook 'org-agenda-list)

;; If you enable Delete Selection mode, a minor mode, then inserting
;; text while the mark is active causes the selected text to be
;; deleted first. This also deactivates the mark.
(delete-selection-mode 1)

;; Emulate vim 'f' and 't'
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c t") 'iy-go-up-to-char)
(global-set-key (kbd "C-c T") 'iy-go-up-to-char-backward)
;; make the every key behave normally after these commands
(setq iy-go-to-char-override-local-map 'nil)

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-line)

(require 'js2-mode)
;; js2-mode as a defalut for js files
(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))
;;TODO
;;autocompletion

;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

;; (load-theme 'solarized-light t)
(load-theme 'solarized-dark t)
