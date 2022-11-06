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
(define-key global-map (kbd "<mouse-8>") 'previous-buffer)
(define-key global-map (kbd "<mouse-9>") 'next-buffer)

(define-key global-map (kbd "C-c b") 'bury-buffer)

;; DISPLAY BUFFER BEHAVIOR
(customize-set-variable
 'display-buffer-base-action
 '((display-buffer-reuse-window display-buffer-same-window)))

(winner-mode)

;; MINIBUFFER COMPLETION
(setq completions-detailed t)

(setq icomplete-compute-delay 0)
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)

;; window TABS
(global-tab-line-mode)

;; SCROLLING BEHAVIOR
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-preserve-screen-position t) ;; don't move cursor when scrolling with C-v and the like
(customize-set-variable 'fast-but-imprecise-scrolling t) ;; allow some imprecision when scrolling fast
;; bind scroll-up-line’ and ‘M-x scroll-down-line’? (the equivalent, respectively, of vi's C-e and C-y)

(tool-bar-mode 0) ;; Don't show tool bar.
;;(menu-bar-mode 0) ;; And menu bar.
(scroll-bar-mode 0) ;; And scroll bar.
(fringe-mode 1) ;; shrink fringes to 1 pixel.

(setq ring-bell-function 'ignore)
;; (setq visible-bell 1) ;; get visual indication

(global-hl-line-mode t) ;; Current line highlighting

;; font
;;

;; tests
;;
;; (add-to-list 'default-frame-alist
;;              '(font . "JetBrains Mono-10"))

;; (set-frame-font 
;;  (concat
;;   (car (remove nil (mapcar (lambda (font) (car (member font (font-family-list))))
;;                            '("JetBrains Mono" "Fira Code" "Menlo" ))))
;;   "-10"))
;;(setq-default line-spacing 5)

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
  (add-to-list 'org-modules "ol-habit")
  (customize-set-variable 'org-habit-show-all-today t)

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

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package olivetti
  :ensure t
  :pin melpa)

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

(use-package magit
  :ensure t
  :config
  (define-key global-map (kbd "C-x g") 'magit-status)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package typescript-mode
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'company-mode)
  (add-hook 'js-mode-hook 'company-mode)
  (add-hook 'js2-mode-hook 'company-mode)
  (add-hook 'html-mode-hook 'company-mode)
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'css-mode-hook 'company-mode))

(setq-default typescript-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

;; (use-package ido-completing-read+
;;   :ensure t
;;   :config
;;   (setq lsp-ido-show-symbol-kind nil
;;         lsp-ido-show-symbol-filename nil))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  ;; (setq yas-snippet-dirs '("~emacs/elpa/yasnippet-snippets-20220713.1234/snippets"))
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


;; (use-package flycheck
;;   :ensure t)

;; (use-package lsp-treemacs
;;   :ensure t)

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-no-png-images t))

(use-package projectile
  :ensure t
  :config
  (define-key global-map (kbd "C-x p C-b") 'projectile-ibuffer))

;; Do not forget to set LSP_USE_PLISTS as true at compile time AND in early-init.el:
;; - https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
;; - https://discourse.doomemacs.org/t/using-lsp-use-plists-with-rust-analyzer-stops-updating-diagnostics-on-save/2832
;;
;; At the moment not using plists breaks renaming (lsp-rename).
;; (setq lsp-keymap-prefix "C-c l")
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((typescript-mode . lsp-deferred)
;; 	 (html-mode . lsp-deferred)
;; 	 (css-mode . lsp-deferred)
;; 	 (js2-mode . lsp-deferred)
;; 	 (js-mode . lsp-deferred)
;; 	 (web-mode . lsp-deferred))
;;   :config
;;   ;; (setq lsp-headerline-breadcrumb-enable nil)
;;   ;; (setq lsp-eldoc-enable-hover nil)
;;   ;; (setq lsp-signature-auto-activate nil)
;;   (setq lsp-headerline-breadcrumb-icons-enable nil)

;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
;;   (define-key lsp-mode-map (kbd "C-<return>") 'lsp-find-definition)
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   (setq gc-cons-threshold 100000000)
;;   (setq read-process-output-max (* 1024 1024))) ;; 1mb)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :config
;;   (define-key lsp-ui-mode-map (kbd "C-c l d") 'lsp-ui-doc-toggle)

;;   ;;(setq lsp-ui-sideline-show-diagnostics nil)
;;   (setq lsp-ui-doc-enable nil) 
;;   (setq lsp-ui-sideline-enable nil))

(setq lsp-keymap-prefix "C-c l")
()

(use-package eglot
  :ensure t
  :config
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  ;;(add-hook 'js2-mode-hook 'eglot-ensure)
  ;;(add-hook 'js-mode-hook 'eglot-ensure)
  ;;(add-hook 'html-mode-hook 'eglot-ensure)
  (add-hook 'css-mode-hook 'eglot-ensure)
  )

(add-to-list 'eglot-server-programs
             '(html-mode "node"
                         "/usr/lib/node_modules/@angular/language-server"
                         "--ngProbeLocations"
                         "/usr/lib/node_modules"
                         "--tsProbeLocations"
                         "/usr/lib/node_modules"
                         "--stdio"))

;; TESTS
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((typescript-mode . lsp)
;; 	 (html-mode . lsp)
;; 	 (css-mode . lsp)
;; 	 (js2-mode . lsp)
;; 	 (js-mode . lsp)
;; 	 (web-mode . lsp))
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil)

;;   ;; (setq lsp-eldoc-enable-hover nil)
;;   ;; (setq lsp-signature-auto-activate nil)
;;   ;;(setq lsp-headerline-breadcrumb-icons-enable nil)

;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

;;   ;;(define-key lsp-mode-map (kbd "C-<return>") 'lsp-find-definition)
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   (setq gc-cons-threshold 100000000)
;;   (setq read-process-output-max (* 1024 1024))) ;; 1mb)

(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)

(use-package undo-tree
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  ;; :preface ; ??
  :config
  (setq evil-default-state 'emacs
        evil-insert-state-modes nil
        evil-motion-state-modes nil)

  ;; Change cursor color in different modes
  ;; https://github.com/bling/dotemacs/blob/master/config/init-evil.el (setq evil-emacs-state-cursor '("grey" box))
  ;; TODO: change color of cursor when it is in the minibuffer

  ;;(setq evil-default-cursor '("#839496" box))

  (setq evil-normal-state-cursor '("#e80000" box))
  (setq evil-emacs-state-cursor '("#839496" box))
  (setq evil-motion-state-cursor '("#e80000" box))
  (setq evil-visual-state-cursor '("#e80000" box))
  (setq evil-insert-state-cursor '("#e80000" bar))
  (setq evil-replace-state-cursor '("#e80000" box))
  (setq evil-operator-state-cursor '("#e80000" hollow))

  ;; ;; The color of the cursor sometimes gets reset to evil-emacs-state-cursor...
  ;; ;;
  ;; ;; trying to debug...
  ;; ;;
  ;; ;; I think that the problem is that certain modes/functions creates
  ;; ;; buffers that start by default in emacs state. This is shown by
  ;; ;; the following message calls:
  ;; ;;
  ;; (add-hook 'evil-emacs-state-entry-hook '(lambda ()
  ;;                                           (message
  ;;                                            (concat "evil-emacs-state-entry-hook" " <" (buffer-name) ">"))))
  ;; (add-hook 'evil-emacs-state-exit-hook '(lambda ()
  ;;                                          (message
  ;;                                           (concat "evil-emacs-state-exit-hook" " <" (buffer-name) ">"))))
  ;; (add-hook 'evil-normal-state-entry-hook '(lambda ()
  ;;                                            (message
  ;;                                             (concat "evil-normal-state-entry-hook" " <" (buffer-name) ">"))))
  ;; (add-hook 'evil-normal-state-exit-hook '(lambda ()
  ;;                                           (message
  ;;                                            (concat "evil-normal-state-exit-hook" " <" (buffer-name) ">"))))

  ;; ;; https://emacs.stackexchange.com/questions/33739/is-there-a-mode-hook-when-you-switch-to-a-buffer-in-eshell-mode
  ;; (add-hook 'buffer-list-update-hook '(lambda ()
  ;;                                     (message
  ;;                                      (concat "buffer-list-update-hook" " <" (buffer-name) ">"))))

  ;; (evil-set-initial-state 'fundamental-mode 'emacs)
  ;; (add-hook 'fundamental-mode-hook '(lambda () (set-cursor-color "#839496")))
  ;; (evil-set-initial-state 'lisp-interaction-mode 'emacs)
  ;; (add-hook 'lisp-interaction-mode-hook '(lambda () (set-cursor-color "#839496")))
  ;; (evil-set-initial-state 'org-mode 'emacs)
  ;; (add-hook 'org-mode-hook '(lambda () (set-cursor-color "#839496")))
  ;; (evil-set-initial-state 'magit-mode 'emacs)
  ;; (add-hook 'magit-mode-hook '(lambda () (set-cursor-color "#839496")))
  ;; (evil-set-initial-state 'help-mode 'emacs)
  ;; (add-hook 'help-mode-hook '(lambda () (set-cursor-color "#839496")))
  ;; (evil-set-initial-state 'Info-mode 'emacs)
  ;; (add-hook 'Info-mode-hook '(lambda () (set-cursor-color "#839496")))


  ;;(evil-set-undo-system 'undo-fu)
  (evil-set-undo-system 'undo-redo)

  ;;(evil-set-initial-state 'typescript-mode 'normal)
  ;; (add-hook 'typescript-mode-hook '(lambda () (set-cursor-color "red")))
  ;;(evil-set-initial-state 'html-mode 'normal)
  ;; (add-hook 'html-mode-hook '(lambda () (set-cursor-color "red")))
  ;;(evil-set-initial-state 'css-mode 'normal)
  ;; (add-hook 'css-mode-hook '(lambda () (set-cursor-color "red")))
  ;;(evil-set-initial-state 'js-mode 'normal)
  ;; (add-hook 'js-mode-hook '(lambda () (set-cursor-color "red")))
  ;;(evil-set-initial-state 'js2-mode 'normal)
  ;; (add-hook 'js2-mode-hook '(lambda () (set-cursor-color "red")))

  (evil-mode 1)

  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil)))))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-bright t))

(use-package modus-themes
  :ensure t)

;; (use-package blamer
;;   :ensure t
;;   ;; :bind (("s-i" . blamer-show-commit-info)
;;   ;;        ("C-c i" . ("s-i" . blamer-show-posframe-commit-info)))
;;   :defer 20
;;   :custom
;;   (blamer-idle-time 0.3)
;;   (blamer-min-offset 70)
;;   :custom-face
;;   (blamer-face ((t :foreground "#7a88cf"
;;                     :background nil
;;                     :height 140
;;                     :italic t)))
;;   ;; :config
;;   ;; (global-blamer-mode 1)
;;   )

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
   '(lsp-treemacs yasnippet-snippets which-key web-mode use-package undo-tree undo-fu typescript-mode switch-window restclient rainbow-delimiters pug-mode projectile pdf-tools paredit org-ref olivetti modus-themes magit lsp-mode impatient-mode ido-completing-read+ helm-bibtex flycheck evil engine-mode dired-subtree dired-narrow company color-theme-sanityinc-tomorrow blamer academic-phrases))
 '(warning-suppress-types '((comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t))))
