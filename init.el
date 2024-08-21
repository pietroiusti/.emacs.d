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

(setq-default truncate-lines t)

(setq indent-tabs-mode nil)

(setq help-window-select t) ;; automatically switch to help window

;; fit windows to buffer horizontally too when using
;; `fit-window-to-buffer`
(setq fit-window-to-buffer-horizontally t)

;; (setq-default show-trailing-whitespace t)
;; (setq-default indicate-empty-lines t)

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

(define-key global-map (kbd "C-c f n") 'flymake-goto-next-error)
(define-key global-map (kbd "C-c f p") 'flymake-goto-prev-error)

;; DISPLAY BUFFER BEHAVIOR
;; (customize-set-variable
;;  'display-buffer-base-action
;;  '((display-buffer-reuse-window display-buffer-same-window)))

(winner-mode)
(define-key global-map (kbd "C-c w <backspace>") 'winner-undo)
(define-key global-map (kbd "C-c w SPC") 'winner-redo)

;; MINIBUFFER COMPLETION
(setq completions-detailed t)

;; (icomplete-mode 1)
;; (icomplete-vertical-mode 1)
;;(setq icomplete-show-matches-on-no-input t)
;(fido-vertical-mode)
;(setq icomplete-compute-delay 0)
;(setq icomplete-delay-completions-threshold 0)
;(setq icomplete-max-delay-chars 0)

;; window TABS
;;(global-tab-line-mode)

;; avoid prompt for dictionary source which defaults to dict.org
(setq dictionary-server "dict.org")

;; modeline
(column-number-mode)

;; SCROLLING BEHAVIOR
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-preserve-screen-position t) ;; don't move cursor when scrolling with C-v and the like
(customize-set-variable 'fast-but-imprecise-scrolling t) ;; allow some imprecision when scrolling fast
;; bind scroll-up-line’ and ‘M-x scroll-down-line’? (the equivalent, respectively, of vi's C-e and C-y)
(setq isearch-allow-scroll 'unlimited)
(setq isearch-lazy-count t)

(tool-bar-mode 0) ;; Don't show tool bar.
(menu-bar-mode 0) ;; And menu bar.
(scroll-bar-mode 0) ;; And scroll bar.
;; (fringe-mode 1) ;; shrink fringes to 1 pixel.

(setq ring-bell-function 'ignore)
;; (setq visible-bell 1) ;; get visual indication

;; font
;;

;; home
;; (add-to-list 'default-frame-alist
;;              '(font . "Inconsolata-11"))
(add-to-list 'default-frame-alist
                      '(font . "Liberation Mono-11"))
(set-face-attribute 'variable-pitch nil :family "Noto sans")

(setq frame-resize-pixelwise t) ;; Remove black border or side gap
                                ;; in certain window managers



;; transparency
;; (set-frame-parameter nil 'alpha-background 85)

;; (add-to-list 'default-frame-alist '(alpha-background . 70))



;; general editing settings
(delete-selection-mode 1) ;; inserting text while the mark is active
;; causes the selected text to be deleted
;; first. This also deactivates the mark.
(show-paren-mode 1) ;; highlight matching parenthesis

;; bookmarks
(setq bookmark-save-flag 1) ;; Save bookmarks every time a bookmark is made or deleted.

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; don't use litte nasty frame

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
          "~/Nextcloud/org/projects.org" "~/Nextcloud/org/inbox.org"
          "~/Nextcloud/org/reminders.org" "~/Nextcloud/org/nextactions.org"
          "~/Nextcloud/org/journal.org" "~/Nextcloud/org/reference.org"
          "~/Nextcloud/org/somedaymaybe.org" "~/Nextcloud/org/zettelkasten.org"))

  ;;(setq org-agenda-start-with-log-mode t)

  (setq org-refile-targets '(( "/home/gp/Nextcloud/org/notes.org" . (:tag . "zkinbox"))))

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
     (C . t)
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
  (pdf-tools-install)
  (setq pdf-view-use-scaling nil))

(use-package olivetti
  :ensure t
  :pin melpa)

;; which-key is in emacs 30 now!
;; (use-package which-key
;;   :ensure t
;;   :config
;;   (which-key-mode))
(which-key-mode)

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
;; (use-package dired-narrow
;;   :ensure t
;;   :config
;;   (bind-keys :map dired-mode-map
;;              ("M-n" . dired-narrow)))
;; (use-package dired-subtree
;;   :ensure t
;;   :config
;;   (bind-keys :map dired-mode-map
;;              ("TAB" . dired-subtree-insert)
;;              ("<S-iso-lefttab>" . dired-subtree-remove)
;;              ("DEL" . dired-subtree-remove)))

(use-package avy
  :ensure t
  :config
  (define-key global-map (kbd "C-;") 'avy-goto-line)
  (define-key global-map (kbd "C-:") 'avy-goto-char))

;; (use-package impatient-mode
;;   :ensure t)

(use-package restclient
  :ensure t)

;; (use-package switch-window
;;   :ensure t
;;   :config
;;   (define-key global-map (kbd "C-c o") 'switch-window))

;; (use-package academic-phrases
;;   :ensure t)

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

(setq c-ts-mode-indent-offset 4)

;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   (setq js2-basic-offset 2)
;;   ;; js2-mode as a defalut for js files
;;   (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode)))

;; (use-package pug-mode
;;   :ensure t)

(use-package magit
  :ensure t
  :config
  (define-key global-map (kbd "C-x g") 'magit-status)
  (setq magit-diff-refine-hunk 'all)
  ;; (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

  ;; Why do I have to add the following line to restore the original
  ;; value? (Only with my build of emacs)
  (setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv)))

;; (use-package typescript-mode
;;   :ensure t
;;   :config)

;; (setq-default typescript-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

;; (use-package ido-completing-read+
;;   :ensure t
;;   :config
;;   (setq lsp-ido-show-symbol-kind nil
;;         lsp-ido-show-symbol-filename nil))

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode)
;;   ;; (setq yas-snippet-dirs '("~emacs/elpa/yasnippet-snippets-20220713.1234/snippets"))
;;   )

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :bind (:map company-active-map
	      ("C-n" . company-select-next-or-abort)
	      ("C-p" . company-select-previous-or-abort))
  :config
  ;;(define-key company-mode-map (kbd "C-<tab>") #'company-complete)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))
 ;; default is 0.2
  ;; (add-hook 'typescript-mode-hook 'company-mode)
  ;; (add-hook 'js-mode-hook 'company-mode)
  ;; (add-hook 'js2-mode-hook 'company-mode)
  ;; (add-hook 'html-mode-hook 'company-mode)
  ;; (add-hook 'web-mode-hook 'company-mode)
  ;; (add-hook 'css-mode-hook 'company-mode))
  ;; (global-company-mode)

  ;; (with-eval-after-load 'company
  ;;   (define-key company-active-map (kbd "M-/") #'company-complete))


;; (use-package flycheck
;;   :ensure t)

;; (use-package lsp-treemacs
;;   :ensure t)

;; (use-package treemacs
;;   :ensure t
;;   :config
;;   (setq treemacs-no-png-images t))

(use-package undo-tree
  :ensure t)

;; (use-package evil
;;   :ensure t
;;   :config
;;   ;; (define-key evil-insert-state-map (kbd "C-<return>") 'evil-normal-state)
;;   ;; (define-key evil-insert-state-map (kbd "<escape>") nil)
;;   (setq evil-disable-insert-state-bindings t)
;;   (setq evil-default-state 'emacs)

;;   (setq evil-normal-state-cursor '("#e80000" box))
;;   (setq evil-insert-state-cursor '("#e80000" bar))
;;   (setq evil-emacs-state-cursor '("#839496" box)))




;; (defun gp/is-buffer-to-ignore (orig-fun &rest args)
;;   ;; (if (string-match "eldoc" (buffer-name))
;;   ;;     (message (buffer-name))
;;   ;;   (message (buffer-name)))

;;   (if (or (string-match "*eldoc" (buffer-name))
;;           (string-match " *temp*" (buffer-name))
;;           (string-match "*org-src-fontification" (buffer-name))
;;           (string-match " markdown-code-fontification:" (buffer-name)))
;;       nil
;;     (apply orig-fun args)))

;; (advice-add 'evil-initialize-state :around #'gp/is-buffer-to-ignore)




;; Possible setting: in insert mode bind 'i' to (evil-emacs-state) and
;; use C-z to switch from emacs state to normal state probably not the
;; best idea: hitting 'i' is not the only context in which we switch
;; to insert mode



;; Possible setting:
;;
;; In insert mode bing escape to escape. so we can do stuff like escape-<
;;
;; Switch to normal state with C-z
;;
;; and we don't need to switch to emacs state because we have already emacs keybinding in insert mode

;; :O :O
;;(defalias 'evil-insert-state 'evil-emacs-state)


(use-package evil
  :ensure t
  :init
  (setq evil-default-state 'emacs)

  (setq evil-motion-state-modes nil)
  (setq evil-insert-state-modes nil)

  (setq evil-disable-insert-state-bindings t)

  (setq evil-normal-state-cursor '("red" box))
  (setq evil-insert-state-cursor '("red" bar))
  ;; (setq evil-emacs-state-cursor '("black" box))
  (setq evil-emacs-state-cursor '("white" box))
  ;;(setq evil-default-cursor '("#e80000" box))

  ;;evil-normal-state-map
  ;; i -> evil-emacs-state
  :config
  (evil-mode 1)

  ;; Dealing with https://github.com/emacs-evil/evil/issues/1835
  (add-hook 'special-mode-hook (lambda () (turn-off-evil-mode)))
  (defun gp/is-buffer-to-ignore (orig-fun &rest args)
    (if (string-match " markdown-code-fontification" (buffer-name))
        nil
      (apply orig-fun args)))
  (advice-add 'evil-initialize-state :around #'gp/is-buffer-to-ignore)


  ;; (define-key evil-insert-state-map (kbd "C-z") 'evil-normal-state)
  ;; (define-key evil-insert-state-map (kbd "<escape>") nil)
  ;; (define-key evil-normal-state-map (kbd "C-z") 'evil-insert-state)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;; avoid insert state.
  ;; ;; Only switch between emacs state and normal state
  ;; sketchy
  ;;(define-key evil-normal-state-map (kbd "i") 'evil-emacs-state)

  ;; how to make insert-state-inducing commands switch to emacs-state
  ;; without screwing up the undo?

  ;; ;; Possible different approach: In order to avoid being in evil mode
  ;; ;; all the time, with `C-z` perhaps we could enable evil mode
  ;; ;; locally to the bufffer, and when hitting `i`, or some
  ;; ;; insert-state-inducing command, just disable the locally activate
  ;; ;; evil-mode.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))


  (evil-set-undo-system 'undo-redo))


;;(debug-on-entry 'evil-normal-state)
;;(debug-on-entry 'set-cursor-color)
;;(debug-on-entry 'evil-change-state)

;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-disable-insert-state-bindings t)
;;   ;; :preface ; ??
;;   :config
;;   (setq evil-default-state 'emacs
;;         evil-insert-state-modes nil
;;         evil-motion-state-modes nil)

;;   ;; Change cursor color in different modes
;;   ;; https://github.com/bling/dotemacs/blob/master/config/init-evil.el (setq evil-emacs-state-cursor '("grey" box))
;;   ;; TODO: change color of cursor when it is in the minibuffer

;;   ;;(setq evil-default-cursor '("#839496" box))






;;   ;; this crap doesn't work as it should
;;   ;; (setq evil-normal-state-cursor '("#e80000" box))
;;   ;; (setq evil-emacs-state-cursor '("#839496" box))
;;   ;; (setq evil-motion-state-cursor '("#e80000" box))
;;   ;; (setq evil-visual-state-cursor '("#e80000" box))
;;   ;; (setq evil-insert-state-cursor '("#e80000" bar))
;;   ;; (setq evil-replace-state-cursor '("#e80000" box))
;;   ;; (setq evil-operator-state-cursor '("#e80000" hollow))









;;   ;; ;; The color of the cursor sometimes gets reset to evil-emacs-state-cursor...
;;   ;; ;;
;;   ;; ;; trying to debug...
;;   ;; ;;
;;   ;; ;; I think that the problem is that certain modes/functions creates
;;   ;; ;; buffers that start by default in emacs state. This is shown by
;;   ;; ;; the following message calls:
;;   ;; ;;
;;   ;; (add-hook 'evil-emacs-state-entry-hook '(lambda ()
;;   ;;                                           (message
;;   ;;                                            (concat "evil-emacs-state-entry-hook" " <" (buffer-name) ">"))))
;;   ;; (add-hook 'evil-emacs-state-exit-hook '(lambda ()
;;   ;;                                          (message
;;   ;;                                           (concat "evil-emacs-state-exit-hook" " <" (buffer-name) ">"))))
;;   ;; (add-hook 'evil-normal-state-entry-hook '(lambda ()
;;   ;;                                            (message
;;   ;;                                             (concat "evil-normal-state-entry-hook" " <" (buffer-name) ">"))))
;;   ;; (add-hook 'evil-normal-state-exit-hook '(lambda ()
;;   ;;                                           (message
;;   ;;                                            (concat "evil-normal-state-exit-hook" " <" (buffer-name) ">"))))

;;   ;; ;; https://emacs.stackexchange.com/questions/33739/is-there-a-mode-hook-when-you-switch-to-a-buffer-in-eshell-mode
;;   ;; (add-hook 'buffer-list-update-hook '(lambda ()
;;   ;;                                     (message
;;   ;;                                      (concat "buffer-list-update-hook" " <" (buffer-name) ">"))))

;;   ;; (evil-set-initial-state 'fundamental-mode 'emacs)
;;   ;; (add-hook 'fundamental-mode-hook '(lambda () (set-cursor-color "#839496")))
;;   ;; (evil-set-initial-state 'lisp-interaction-mode 'emacs)
;;   ;; (add-hook 'lisp-interaction-mode-hook '(lambda () (set-cursor-color "#839496")))
;;   ;; (evil-set-initial-state 'org-mode 'emacs)
;;   ;; (add-hook 'org-mode-hook '(lambda () (set-cursor-color "#839496")))
;;   ;; (evil-set-initial-state 'magit-mode 'emacs)
;;   ;; (add-hook 'magit-mode-hook '(lambda () (set-cursor-color "#839496")))
;;   ;; (evil-set-initial-state 'help-mode 'emacs)
;;   ;; (add-hook 'help-mode-hook '(lambda () (set-cursor-color "#839496")))
;;   ;; (evil-set-initial-state 'Info-mode 'emacs)
;;   ;; (add-hook 'Info-mode-hook '(lambda () (set-cursor-color "#839496")))


;;   ;;(evil-set-undo-system 'undo-fu)
;;   (evil-set-undo-system 'undo-redo)

;;   ;;(evil-set-initial-state 'typescript-mode 'normal)
;;   ;; (add-hook 'typescript-mode-hook '(lambda () (set-cursor-color "red")))
;;   ;;(evil-set-initial-state 'html-mode 'normal)
;;   ;; (add-hook 'html-mode-hook '(lambda () (set-cursor-color "red")))
;;   ;;(evil-set-initial-state 'css-mode 'normal)
;;   ;; (add-hook 'css-mode-hook '(lambda () (set-cursor-color "red")))
;;   ;;(evil-set-initial-state 'js-mode 'normal)
;;   ;; (add-hook 'js-mode-hook '(lambda () (set-cursor-color "red")))
;;   ;;(evil-set-initial-state 'js2-mode 'normal)
;;   ;; (add-hook 'js2-mode-hook '(lambda () (set-cursor-color "red")))

;;   (evil-mode 1)

;;   (evil-set-initial-state 'pdf-view-mode 'emacs)
;;   (add-hook 'pdf-view-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'evil-emacs-state-cursor) (list nil)))))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package modus-themes
  :ensure t)

(load-theme 'modus-operandi t)

;; EXWM
;; I keep a separate file that is loaded only when Emacs works as X WM.
;; In my .xinitrc I have something like:
;; exec dbus-launch --exit-with-session emacs -l ~/.emacs.d/exwm.el






(setq enable-recursive-minibuffers t)

;; NEW STUFF ##################################################
;;
;; lsp
(setq eglot-events-buffer-size 0);; makes tsserver usable...
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)

;; Disable inlay hints
(add-hook 'eglot-managed-mode-hook '(lambda () (eglot-inlay-hints-mode -1)))

;; (add-to-list 'eglot-server-programs
;;              '(html-mode "node"
;;                          "/usr/lib/node_modules/@angular/language-server"
;;                          "--ngProbeLocations"
;;                          "/usr/lib/node_modules"
;;                          "--tsProbeLocations"
;;                          "/usr/lib/node_modules"
;;                          "--stdio"))

;; scrolling stuff
(pixel-scroll-precision-mode)
(setq isearch-allow-scroll 'unlimited)

;; tests to improve performance
;; see https://emacs-lsp.github.io/lsp-mode/page/performance/
;; and https://joaotavora.github.io/eglot/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; I hate the minibuffer expanding so rudely
(setq eldoc-echo-area-use-multiline-p nil)

;; language modes
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;;(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)) ;; why doesn't this work?
(add-to-list 'auto-mode-alist '("\\.ts?$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js?$" . js-ts-mode))

(add-hook 'typescript-ts-mode-hook '(lambda () (display-line-numbers-mode)))
(add-hook 'js-ts-mode-hook '(lambda () (display-line-numbers-mode)))

(add-hook 'c-ts-mode-hook '(lambda () (display-line-numbers-mode)))

(setq native-comp-async-report-warnings-errors nil)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode)
  (custom-set-variables '(git-gutter:update-interval 2)))







;; Enable vertico
(use-package vertico
  :ensure t
  ;; :bind
  ;; (("C-j" . 'vertico-insert))

  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)

  :config
  ;;(define-key vertico-map (kbd "C-m") 'vertico-directory-enter) ;; ido-like behavior.
  ;; I moved this commented config below following documentation's reccomendation
  (define-key vertico-map (kbd "<return>") 'vertico-exit)
  (define-key vertico-map (kbd "C-j") 'vertico-exit)
  (define-key vertico-map (kbd "M-j") 'vertico-exit-input))

;; (f)ido-like behavior
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("C-m" . 'vertico-directory-enter)))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless flex basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

  ;; case (in)sensitivity
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))








(use-package marginalia
  :ensure t

  :config
  (marginalia-mode)

  ;; https://github.com/minad/marginalia/issues/142
  (add-hook 'icomplete-minibuffer-setup-hook
          (lambda () (setq truncate-lines t))))

(use-package consult
  :ensure t
  :config
  (define-key global-map (kbd "C-c l") 'consult-line)
  (define-key global-map (kbd "C-c s") 'consult-ripgrep))

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion))))

;;   (defun my-icomplete-styles ()
;;     (setq-local completion-styles '(orderless flex)))
;;   (add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles))

(use-package embark
  :ensure t

  :bind

  (("C-c e a" . embark-act)         ;; pick some comfortable binding
   ("C-c e d" . embark-dwim)        ;; good alternative: M-.
   ("C-c e h" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

;;   ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;;   ;; strategy, if you want to see the documentation from multiple providers.
;;   (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;;   ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

;;   :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t)

(use-package fold-this
  :ensure t)

(use-package eldoc-box
  :ensure t
  :bind
  (("C-c h". eldoc-box-help-at-point)))

(use-package vterm
  :ensure t)

(use-package eat
  :ensure t)



;; (require 'keyfreq)
;; (keyfreq-mode 1)
;; (keyfreq-autosave-mode 1)


;(load-file "./meow.el")

;;
;; NEW STUFF ##################################################
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gp-reverse))
 '(custom-safe-themes
   '("488bb61b41c7bab150d07965dd513117ce2b7c007d3b78ab2c174255779e5f6d" default))
 '(git-gutter:update-interval 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
