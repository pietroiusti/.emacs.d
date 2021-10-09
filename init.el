(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/"))))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq user-full-name "Giulio Pietroiusti"
      user-mail-address "giulio.pietroiusti@gmail.com")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq make-backup-files nil) ;; Disable backup.
(setq auto-save-default nil) ;; And auto-save.

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y/n instead of yes/no
(setq disabled-command-function nil) ;; enable all commands

(define-key global-map (kbd "C-x C-b") 'ibuffer)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1 scroll-conservatively  10000)

(tool-bar-mode 0) ;; Don't show tool bar.
(menu-bar-mode 0) ;; And menu bar.
(scroll-bar-mode 0) ;; And scroll bar.
(fringe-mode 1) ;; shrink fringes to 1 pixel.

(setq visible-bell 1) ;; get visual indication

(global-hl-line-mode t) ;; Current line highlighting

;; font
(set-face-attribute 'default nil :family "Inconsolata" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Inconsolata")
(set-face-attribute 'variable-pitch nil :family "Noto sans")

(setq frame-resize-pixelwise t) ;; Remove black border at the bottom
;; in certain window managers

;; fancy mode line
(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))
(define-key global-map (kbd "C-c x") 'toggle-transparency)

;; general editing settings
(delete-selection-mode 1) ;; inserting text while the mark is active
;; causes the selected text to be deleted
;; first. This also deactivates the mark.
(show-paren-mode 1) ;; highlight matching parenthesis

;; bookmarks
(setq bookmark-save-flag 1) ;; Save bookmarks every time a bookmark is made or deleted.

(use-package org
  ;; TODO: use-package, even with ":pin org", doesn't install the org
  ;; version, but keeps the built-in version.
  ;; :pin org
  :ensure t
  :config
  (setq org-startup-indented t)
  (setq org-indent-mode t)
  (setq org-hide-leading-stars t)
  (setq org-ellipsis "▼")
  (setq org-src-fontify-natively t)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-ctrl-k-protect-subtree t)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-ca" 'org-agenda)

  (setq org-capture-templates
        '(
	  ("i" "Add to inbox" entry
           (file "~/Nextcloud/org/inbox.org")
           "* TODO %?" :prepend t)
	  
          ("t" "Add todo" entry
           (file "~/Nextcloud/org/todo.org")
           "* TODO %?" :prepend t)

          ("n" "Add note" entry
           (file "~/Nextcloud/org/notes.org")
           "* %?" :prepend t)

          ("r" "Add reading" entry
           (file "~/Nextcloud/org/readings.org")
           "* %?" :prepend t)

          ("a" "Add activity" entry
           (file "~/Nextcloud/org/activities.org")
           "* %?")

	  ("p" "Add project" entry
           (file "~/Nextcloud/org/projects.org")
           "* %?" :prepend t)

          ("m" "Add meeting")
          ("md" "Dan" entry
           (file "~/Nextcloud/org/meetings.org")
           "* Dan \n SCHEDULED: %^t%?")
          ("mt" "Teresa" entry
           (file "~/Nextcloud/org/meetings.org")
           "* Teresa \n SCHEDULED: %^t%?")
          ("mo" "Other" entry
           (file "~/Nextcloud/org/meetings.org")
           "* %?")))

  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "DONE" )))

  ;; add timestamp to item when marked as DONE
  (setq org-log-done 'time)

  (setq org-agenda-files
        '("~/Nextcloud/org/activities.org" "~/Nextcloud/org/todo.org"
          "~/Nextcloud/org/meetings.org" "~/Nextcloud/org/notes.org"
          "~/Nextcloud/org/readings.org" "~/Nextcloud/org/teaching.org"
          "~/Nextcloud/org/habits.org" "~/Nextcloud/org/workouts.org"
	  "~/Nextcloud/org/projects.org" "~/Nextcloud/org/inbox.org"))

  ;;(setq org-agenda-start-with-log-mode t)

  (require 'org-habit)
  (add-to-list 'org-modules "org-habit")

  (require 'org-id)
  
  ;; code block evaluation
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (js . t)))

  ;; I use the following to export to pdf
  ;; At the beginning of the org files I have:
  ;; #+LATEX_HEADER: \usepackage[backend=biber,style=authoryear]{biblatex}
  ;; #+LATEX_HEADER: \addbibresource{references.bib}
  ;;
  ;; And at the end:
  ;; \printbibliography
  ;;
  ;; If I want to print also those refs I haven't used, before \printbibliography:
  ;; \nocite{*}
  (setq org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "biber %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(use-package org-ref
  :ensure t
  :after org
  :config
  (setq reftex-default-bibliography '("~/Nextcloud/org/references.bib"))

  (setq org-ref-default-bibliography '("~/Nextcloud/org/references.bib")
	org-ref-pdf-directory "~/Nextcloud/org/pdfs/"
	org-ref-bibliography-notes "~/Nextcloud/org/readings.org")

  (setq bibtex-completion-bibliography "~/Nextcloud/org/references.bib"
	bibtex-completion-library-path "~/Nextcloud/org/pdfs/")

  ;; open pdf with pdf-tools
  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file pdf-file)
	(message "No PDF found for %s" key))))

  (setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point))

(use-package org-tree-slide
  :ensure t
  :after org
  :config
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-skip-outline-level 0)
  (setq org-tree-slide-modeline-display 'outside)

  (define-minor-mode gp/org-presentation-mode
    "Parameters for plain text presentations with `org-mode'."
    :init-value nil
    :global nil
    (if gp/org-presentation-mode
        (progn
          (unless (eq major-mode 'org-mode)
            (user-error "Not in an Org buffer"))
          (org-tree-slide-mode 1)
          (olivetti-mode 1)
          (variable-pitch-mode 1))
      (org-tree-slide-mode -1)
      (olivetti-mode -1)
      (variable-pitch-mode -1)))

  ;; how to hide org emphasis LOCALLY to buffer?
  ;; (setq org-hide-emphasis-markers t)

  :bind (("M-C-q" . gp/org-presentation-mode)
         :map org-tree-slide-mode-map
         ("<C-down>" . org-tree-slide-display-header-toggle)
         ("<right>" . org-tree-slide-move-next-tree)
         ("<left>" . org-tree-slide-move-previous-tree)))

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)

  (setq evil-default-state 'emacs
        evil-insert-state-modes nil
        evil-motion-state-modes nil)

  ;; solve blinking problem with pdf-tools
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))

  ;; Change cursor color in different modes
  ;; https://github.com/bling/dotemacs/blob/master/config/init-evil.el (setq evil-emacs-state-cursor '("grey" box))
  ;; TODO: change color of cursor when it is in the minibuffer
  (setq evil-emacs-state-cursor '("#839496" box))
  (setq evil-motion-state-cursor '("#e80000" box))
  (setq evil-normal-state-cursor '("#e80000" box))
  (setq evil-visual-state-cursor '("#e80000" box))
  (setq evil-insert-state-cursor '("#e80000" bar))
  (setq evil-replace-state-cursor '("#e80000" bar))
  (setq evil-operator-state-cursor '("#e80000" hollow)))

(use-package magit
  :ensure t
  :config
  (define-key global-map (kbd "C-x g") 'magit-status))

(use-package pdf-tools
  :ensure t
  :pin melpa
  :config
  (pdf-tools-install)

  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "K") 'image-kill-buffer))

(use-package olivetti
  :ensure t
  :pin melpa)

;; (use-package auctex
;;     :ensure t
;;     :config

;;     ;; from the manual: If you want to make AUCTeX aware of style files and multi-file
;;     ;; documents right away, insert the following in your '.emacs' file.
    
;;     (setq TeX-auto-save t)
;;     (setq Tex-parse-self t)
;;     (setq-default TeX-master nil))

(use-package company
    :ensure t
    :config
    (add-hook 'after-init-hook 'global-company-mode))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :config
					;(setq ivy-use-virtual-buffers t)
					;(setq ivy-count-format "(%d/%d) ")
					;(setq enable-recursive-minibuffers t)
  
  ;; (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done) ; why does it bind C-m to ivy-alt-done as well?
  
  ;; :bind (:map ivy-minibuffer-map
  ;;        ("RET" . ivy-alt-done)) ; why does it bind C-m to ivy-alt-done as well?
  )

(use-package ivy-prescient
  :ensure t
  :after (ivy)
  :init
  (ivy-prescient-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)
  (defengine sci-hub
    "https://sci-hub.se/%s/"
    :keybinding "s")

  (defengine oxforddictionaries
    "https://en.oxforddictionaries.com/definition/%s/"
    :keybinding "o")

  (defengine cambridgedictionaries
    "https://dictionary.cambridge.org/dictionary/english/%s/"
    :keybinding "c")
  
  (defengine google-translate
    "https://translate.google.com/#en/it/%s/"
    :keybinding "t")

  (defengine treccanivocabolario
    "http://www.treccani.it/vocabolario/ricerca/%s/"
    :keybinding "i"))

;; dired
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(use-package dired-narrow
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("M-n" . dired-narrow)))
(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("TAB" . dired-subtree-insert)
             ("<S-iso-lefttab>" . dired-subtree-remove)
             ("DEL" . dired-subtree-remove)))

(use-package avy
  :ensure t
  :config
  (define-key global-map (kbd "C-;") 'avy-goto-line)
  (define-key global-map (kbd "C-:") 'avy-goto-char))

(use-package impatient-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Nextcloud/org/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package switch-window
  :ensure t
  :config
  (define-key global-map (kbd "C-c o") 'switch-window))

(use-package buffer-move ;; see exwm config
  :ensure t)

(use-package elfeed
    :ensure t
    :config
    (define-key global-map (kbd "C-x w") 'elfeed)
    (setq elfeed-feeds
        '(("https://www.stallman.org/rss/rss.xml" stallman)
          ("https://planet.emacslife.com/atom.xml" emacs))))

(use-package academic-phrases
  :ensure t)

;; languages config stuff

(use-package paredit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (paredit-mode t)
;;             (rainbow-delimiters-mode t)
;;             (show-paren-mode 1)
;;             ))
;; (add-hook 'lisp-interaction-mode
;;           (lambda ()
;;             (paredit-mode t)
;;             (rainbow-delimiters-mode t)
;;             (show-paren-mode 1)
;;             ))

(setq-default c-default-style "linux"
              c-basic-offset 4)

(use-package js2-mode
  :ensure t
  :config
  (setq js2-basic-offset 2)
  ;; js2-mode as a defalut for js files
  (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode)))

(use-package pug-mode
  :ensure t)

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

;; EXWM
;; I keep a separate file that is loaded only when Emacs works as X WM.
;; In my .xinitrc I have something like:
;; exec dbus-launch --exit-with-session emacs -l ~/.emacs.d/exwm.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exwm pug-mode js2-mode rainbow-delimiters paredit academic-phrases elfeed buffer-move switch-window restclient impatient-mode avy dired-subtree dired-narrow engine-mode web-mode which-key ivy-prescient ivy company auctex olivetti pdf-tools magit evil org-tree-slide color-theme-sanityinc-tomorrow doom-modeline ibuffer-projectile use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
