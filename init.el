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

(let ((user (getenv "USER")))
  (when (stringp user)
    (load (concat "/home/" user "/.emacs.d/custom-functions.el"))
    (load (concat "/home/" user "/.emacs.d/buffer-move.el"))))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq make-backup-files nil) ;; Disable backup.
(setq auto-save-default nil) ;; And auto-save.

(fset 'yes-or-no-p 'y-or-n-p) ;; Use y/n instead of yes/no
(setq disabled-command-function nil) ;; enable all commands

(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-c s") 'shell)
(define-key global-map (kbd "C-c k") 'kill-current-buffer)
(define-key global-map (kbd "C-c i") 'indent-region)
(define-key global-map (kbd "C-c r") 'revert-buffer)

(define-key global-map (kbd "C-c w h") 'windmove-left)
(define-key global-map (kbd "C-c w b") 'windmove-left)
(define-key global-map (kbd "C-c w l") 'windmove-right)
(define-key global-map (kbd "C-c w f") 'windmove-right)
(define-key global-map (kbd "C-c w j") 'windmove-down)
(define-key global-map (kbd "C-c w n") 'windmove-down)
(define-key global-map (kbd "C-c w k") 'windmove-up)
(define-key global-map (kbd "C-c w p") 'windmove-up)

(define-key global-map (kbd "C-c w C-b") 'buf-move-left)
(define-key global-map (kbd "C-c w C-f") 'buf-move-right)
(define-key global-map (kbd "C-c w C-n") 'buf-move-down)
(define-key global-map (kbd "C-c w C-p") 'buf-move-up)

(define-key global-map (kbd "C-c p") 'previous-buffer)
(define-key global-map (kbd "C-c n") 'next-buffer)

;; keyboard scrolling
(setq scroll-step 1)
(setq scroll-conservatively 1001)

(tool-bar-mode 0) ;; Don't show tool bar.
(menu-bar-mode 0) ;; And menu bar.
(scroll-bar-mode 0) ;; And scroll bar.
(fringe-mode 1) ;; shrink fringes to 1 pixel.

(setq ring-bell-function 'ignore)
;; (setq visible-bell 1) ;; get visual indication

(global-hl-line-mode t) ;; Current line highlighting

;; font
;;
;; home
(add-to-list 'default-frame-alist
                      '(font . "Inconsolata-11"))
(set-face-attribute 'variable-pitch nil :family "Noto sans")
;;
;; work:
;; (add-to-list 'default-frame-alist
;;              '(font . "Inconsolata-10"))

;;(set-face-attribute 'default nil :family "Inconsolata")
;;(set-face-attribute 'fixed-pitch nil :family "Inconsolata")
(set-face-attribute 'variable-pitch nil :family "Noto sans")


(setq frame-resize-pixelwise t) ;; Remove black border or side gap
                                ;; in certain window managers

;; old ###################################################################
;;(set-frame-font "Inconsolata-10")
;;(set-face-attribute 'default t :font "Inconsolata-10")
;;(set-face-attribute 'default nil :family "Inconsolata" :height 120)

;;(set-frame-font "DejaVu Sans Mono-9")
;;(set-face-attribute 'default t :font "DejaVu Sans Mono-9")
;;(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")

;;(set-face-attribute 'fixed-pitch nil :family "Inconsolata")
;;(set-frame-font "Inconsolata-10")

;;(set-face-attribute 'fixed-pitch nil :family "Inconsolata")
;;(set-face-attribute 'variable-pitch nil :family "Noto sans")
;; old ###################################################################

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq-default typescript-indent-level 2)

(use-package typescript-mode
  :ensure t)

;; fancy mode line
(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  ;; (load-theme 'sanityinc-tomorrow-bright t)
  (load-theme 'modus-operandi t))

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

;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
;; (defun toggle-transparency ()
;;   (interactive)
;;   (let ((alpha (frame-parameter nil 'alpha)))
;;     (set-frame-parameter
;;      nil 'alpha
;;      (if (eql (cond ((numberp alpha) alpha)
;;                     ((numberp (cdr alpha)) (cdr alpha))
;;                     ;; Also handle undocumented (<active> <inactive>) form.
;;                     ((numberp (cadr alpha)) (cadr alpha)))
;;               100)
;;          '(85 . 85) '(100 . 100)))))
;; (define-key global-map (kbd "C-c x") 'toggle-transparency)

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
  ;;(setq org-stars)
  (setq org-src-fontify-natively t)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-ctrl-k-protect-subtree t)

  ;; In case you want to resize images, you might want uncomment:
  ;; (setq org-image-actual-width nil)

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

  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . emacs ))

  (require 'ox-texinfo)

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
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (add-to-list 'org-latex-classes
               '("myreport"
                 "\\documentclass[11pt]{report}"
                 ;; ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package helm
  :ensure t
  :config
  ;; (global-set-key (kbd "M-x") #'helm-M-x)
  ;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  ;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
  ;; (helm-mode 1)
  )

(use-package helm-bibtex
  :ensure t
  :after org)

(use-package org-ref
  :ensure t
  :after org
  :config

  (setq bibtex-completion-bibliography '("~/Nextcloud/org/references.bib")
        bibtex-completion-library-path '("~/Nextcloud/org/pdfs/")
        bibtex-completion-notes-path "~/Nextcloud/org/readings.org"
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath)))
  
  (require 'org-ref-helm)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
        org-ref-insert-cite-function 'org-ref-cite-insert-helm
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link
        org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
  
  (define-key org-mode-map (kbd "M-]") 'org-ref-insert-link))

;; (use-package diminish
;;   :ensure t)

;; Some lisp to prettify org blocks from https://pank.eu/blog/pretty-babel-src-blocks.html
;; I'm using this for presentations in vanilla org-mode
(defvar-local rasmus/org-at-src-begin -1
  "Variable that holds whether last position was a ")

(defvar rasmus/ob-header-symbol ?☰
  "Symbol used for babel headers")

(defun rasmus/org-prettify-src--update ()
  (let ((case-fold-search t)
        (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
        found)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (goto-char (match-end 0))
        (let ((args (org-trim
                     (buffer-substring-no-properties (point)
                                                     (line-end-position)))))
          (when (org-string-nw-p args)
            (let ((new-cell (cons args rasmus/ob-header-symbol)))
              (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
              (cl-pushnew new-cell found :test #'equal)))))
      (setq prettify-symbols-alist
            (cl-set-difference prettify-symbols-alist
                               (cl-set-difference
                                (cl-remove-if-not
                                 (lambda (elm)
                                   (eq (cdr elm) rasmus/ob-header-symbol))
                                 prettify-symbols-alist)
                                found :test #'equal)))
      ;; Clean up old font-lock-keywords.
      (font-lock-remove-keywords nil prettify-symbols--keywords)
      (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
      (font-lock-add-keywords nil prettify-symbols--keywords)
      (while (re-search-forward re nil t)
        (font-lock-flush (line-beginning-position) (line-end-position))))))

(defun rasmus/org-prettify-src ()
  "Hide src options via `prettify-symbols-mode'.

  `prettify-symbols-mode' is used because it has uncollpasing. It's
  may not be efficient."
  (let* ((case-fold-search t)
         (at-src-block (save-excursion
                         (beginning-of-line)
                         (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
    ;; Test if we moved out of a block.
    (when (or (and rasmus/org-at-src-begin
                   (not at-src-block))
              ;; File was just opened.
              (eq rasmus/org-at-src-begin -1))
      (rasmus/org-prettify-src--update))
    ;; Remove composition if at line; doesn't work properly.
    ;; (when at-src-block
    ;;   (with-silent-modifications
    ;;     (remove-text-properties (match-end 0)
    ;;                             (1+ (line-end-position))
    ;;                             '(composition))))
    (setq rasmus/org-at-src-begin at-src-block)))

(defun rasmus/org-prettify-symbols ()
  (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
        (cl-reduce 'append
                   (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                           `(("#+begin_src" . ?✎) ;; ➤ 🖝 ➟ ➤ ✎
                             ("#+end_src"   . ?□) ;; ⏹
                             ("#+header:" . ,rasmus/ob-header-symbol)
                             ("#+begin_quote" . ?«)
                             ("#+end_quote" . ?»)))))
  (turn-on-prettify-symbols-mode)
  (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))

;; vanilla org-mode presentations setup
(defun gp/presentation-start ()
  (interactive)
  (setq org-hide-emphasis-markers t)
  (org-mode-restart)
  (variable-pitch-mode)
  (olivetti-mode)
  (rasmus/org-prettify-symbols))

(use-package org-tree-slide
  :ensure t
  :config
  (setq org-tree-slide-slide-in-effect t)

  (setq org-tree-slide-activate-message "Hello!")
  (setq org-tree-slide-deactivate-message "Bye!")
  
  (defun gp/presentation-setup()
    ;; (variable-pitch-mode)
    ;; (olivetti-mode)
    )

  (defun gp/presentation-end()
    ;; (variable-pitch-mode -1)
    ;; (olivetti-mode -1)
    )

  ;; (setq org-tree-slide-cursor-init nil)
  ;; (setq org-tree-slide-modeline-display 'outside)

  (add-hook 'org-tree-slide-play-hook 'gp/presentation-setup)
  (add-hook 'org-tree-slide-stop-hook 'gp/presentation-end)

  (define-key global-map (kbd "<f8>") 'org-tree-slide-mode)
  (define-key org-tree-slide-mode-map (kbd "<next>")
    'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map (kbd "<prior>")
    'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f9>")
    'org-tree-slide-content))

;; org-tree-slide-content

;; TESTING
;; (use-package org-roam
;;   :ensure t
;;   :config
;;   (setq org-roam-v2-ack t)
;;   (setq org-roam-directory (file-truename "~/Nextcloud/org-roam-test"))
;;   (org-roam-db-autosync-mode))

(use-package undo-fu
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)

  (setq evil-default-state 'emacs
        evil-insert-state-modes nil
        evil-motion-state-modes nil)

  (evil-set-undo-system 'undo-fu)

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
  (setq evil-replace-state-cursor '("#e80000" box))
  (setq evil-operator-state-cursor '("#e80000" hollow)))


(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "C-<escape>") #'god-local-mode))

(use-package magit
  :ensure t
  :config
  (define-key global-map (kbd "C-x g") 'magit-status))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; (use-package pdf-tools
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (pdf-tools-install)

;;   (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
;;   (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
;;   (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
;;   (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
;;   (define-key pdf-view-mode-map (kbd "K") 'image-kill-buffer))

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

;; (use-package company
;;     :ensure t
;;     :config
;;     (add-hook 'after-init-hook 'global-company-mode))

;; (use-package ivy
;;   :ensure t
;;   :init
;;   (ivy-mode 1)
;;   :config
;;                                         ;(setq ivy-use-virtual-buffers t)
;;                                         ;(setq ivy-count-format "(%d/%d) ")
;;                                         ;(setq enable-recursive-minibuffers t)
  
;;   ;; (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done) ; why does it bind C-m to ivy-alt-done as well?
  
;;   ;; :bind (:map ivy-minibuffer-map
;;   ;;        ("RET" . ivy-alt-done)) ; why does it bind C-m to ivy-alt-done as well?
;;   )

;; (use-package ivy-prescient
;;   :ensure t
;;   :after (ivy)
;;   :init
;;   (ivy-prescient-mode 1))

;; Since emacs 28.1 we have fido-vertical-mode... It seems to work
;; very well!
(fido-vertical-mode)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package web-mode
  :ensure t
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

  ;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

  (defun web-mode-init-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'web-mode-init-hook))
 
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

(use-package switch-window
  :ensure t
  :config
  (define-key global-map (kbd "C-c o") 'switch-window))

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

;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   (setq js2-basic-offset 2)
;;   ;; js2-mode as a defalut for js files
;;   (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode)))

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

(defun gp/set-key-rate ()
  (interactive)
  (shell-command "xset r rate 200 60"))
(gp/set-key-rate)

;; (use-package eglot
;;   :ensure t)

;; (use-package dap-mode
;;   :ensure t)

(setenv "LSP_USE_PLISTS" "1")
;; Do not forget to set LSP_USE_PLISTS as true also at compile time:
;; - https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
;; - https://discourse.doomemacs.org/t/using-lsp-use-plists-with-rust-analyzer-stops-updating-diagnostics-on-save/2832
;;
;; At the moment not using plists breaks renaming (lsp-rename).
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  (require 'lsp-ido)
  (setq lsp-eldoc-render-all nil)
  (bind-keys :map lsp-mode-map
             ("C-<return>" . lsp-find-definition)))

(use-package ido-completing-read+
  :ensure t
  :config
  (setq lsp-ido-show-symbol-kind nil
        lsp-ido-show-symbol-filename nil))

(use-package yasnippet
  :ensure t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config

  ;;see https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  
  ;; disable doc on mouse hover
  ;; https://github.com/emacs-lsp/lsp-ui/issues/523
  (setq lsp-ui-doc-enable nil) 
  
  ;; disable headerline
  (setq lsp-headerline-breadcrumb-enable nil)

  
  ;;(setq lsp-ui-sideline-enable nil)
  ;;(setq lsp-ui-sideline-show-code-actions nil) ;; ??
  (setq lsp-ui-sideline-show-diagnostics nil) ;; hide only (side) errors?


  ;;(setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-enable-hover nil)

  ;;(setq lsp-modeline-diagnostics-enable nil)

  (setq lsp-signature-auto-activate nil) ;; you can manually request them via `lsp-signature-activate`
                                         ;; C-c l h s
  )

(use-package company
  :ensure t
  ;; :hook (lsp-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2
  )
  ;; (global-company-mode)

  ;; (with-eval-after-load 'company
  ;;   (define-key company-active-map (kbd "M-/") #'company-complete))


(use-package flycheck
  :ensure t)

;; (use-package lsp-treemacs
;;   :ensure t)

(use-package projectile
  :ensure t
  :config
  (define-key global-map (kbd "C-x p C-b") 'projectile-ibuffer))

;; (use-package ng2-mode
;;   :ensure t)

;; EXWM
;; I keep a separate file that is loaded only when Emacs works as X WM.
;; In my .xinitrc I have something like:
;; exec dbus-launch --exit-with-session emacs -l ~/.emacs.d/exwm.el
