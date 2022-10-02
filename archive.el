transparency
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

;; (use-package diminish
;;   :ensure t)

;; Some lisp to prettify org blocks from https://pank.eu/blog/pretty-babel-src-blocks.html
;; I'm using this for presentations in vanilla org-mode
;; 
;; (defvar-local rasmus/org-at-src-begin -1
;;   "Variable that holds whether last position was a ")

;; (defvar rasmus/ob-header-symbol ?☰
;;   "Symbol used for babel headers")

;; (defun rasmus/org-prettify-src--update ()
;;   (let ((case-fold-search t)
;;         (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
;;         found)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (re-search-forward re nil t)
;;         (goto-char (match-end 0))
;;         (let ((args (org-trim
;;                      (buffer-substring-no-properties (point)
;;                                                      (line-end-position)))))
;;           (when (org-string-nw-p args)
;;             (let ((new-cell (cons args rasmus/ob-header-symbol)))
;;               (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
;;               (cl-pushnew new-cell found :test #'equal)))))
;;       (setq prettify-symbols-alist
;;             (cl-set-difference prettify-symbols-alist
;;                                (cl-set-difference
;;                                 (cl-remove-if-not
;;                                  (lambda (elm)
;;                                    (eq (cdr elm) rasmus/ob-header-symbol))
;;                                  prettify-symbols-alist)
;;                                 found :test #'equal)))
;;       ;; Clean up old font-lock-keywords.
;;       (font-lock-remove-keywords nil prettify-symbols--keywords)
;;       (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
;;       (font-lock-add-keywords nil prettify-symbols--keywords)
;;       (while (re-search-forward re nil t)
;;         (font-lock-flush (line-beginning-position) (line-end-position))))))

;; (defun rasmus/org-prettify-src ()
;;   "Hide src options via `prettify-symbols-mode'.

;;   `prettify-symbols-mode' is used because it has uncollpasing. It's
;;   may not be efficient."
;;   (let* ((case-fold-search t)
;;          (at-src-block (save-excursion
;;                          (beginning-of-line)
;;                          (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
;;     ;; Test if we moved out of a block.
;;     (when (or (and rasmus/org-at-src-begin
;;                    (not at-src-block))
;;               ;; File was just opened.
;;               (eq rasmus/org-at-src-begin -1))
;;       (rasmus/org-prettify-src--update))
;;     ;; Remove composition if at line; doesn't work properly.
;;     ;; (when at-src-block
;;     ;;   (with-silent-modifications
;;     ;;     (remove-text-properties (match-end 0)
;;     ;;                             (1+ (line-end-position))
;;     ;;                             '(composition))))
;;     (setq rasmus/org-at-src-begin at-src-block)))

;; (defun rasmus/org-prettify-symbols ()
;;   (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
;;         (cl-reduce 'append
;;                    (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
;;                            `(("#+begin_src" . ?✎) ;; ➤ 🖝 ➟ ➤ ✎
;;                              ("#+end_src"   . ?□) ;; ⏹
;;                              ("#+header:" . ,rasmus/ob-header-symbol)
;;                              ("#+begin_quote" . ?«)
;;                              ("#+end_quote" . ?»)))))
;;   (turn-on-prettify-symbols-mode)
;;   (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))

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

;; (use-package auctex
;;     :ensure t
;;     :config

;;     ;; from the manual: If you want to make AUCTeX aware of style files and multi-file
;;     ;; documents right away, insert the following in your '.emacs' file.

;;     (setq TeX-auto-save t)
;;     (setq Tex-parse-self t)
;;     (setq-default TeX-master nil))

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
