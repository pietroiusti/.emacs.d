;; implement ability to ``undo'' the action performed by
;; evil-scroll-page-down and evil-scroll-page-up. Useful when pressing
;; C-f and C-b in evil mode when wanting to move a char
;; forward/backward, rather than scrolling.

;; see https://github.com/pietroiusti/writing-gnu-emacs-extensions
;; (notes on chapter 3)

(defadvice evil-scroll-page-down (before remember-for-unscroll
                                         activate compile)
  "Remember where we started from, for unscroll."
  (unscroll-maybe-remember))

(defadvice evil-scroll-page-up (before remember-for-unscroll
                                       activate compile)
  "Remember where we started from, for unscroll."
  (unscroll-maybe-remember))

;; add 'unscrollable to symbol's properties
(put 'evil-scroll-page-down 'unscrollable t)
(put 'evil-scroll-page-up 'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to 'unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to 'unscroll'.")

(defun unscroll-maybe-remember ()
  (if (not (get real-last-command 'unscrollable))
      (progn
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll (window-hscroll)))))

(defun unscroll ()
  "Revert to 'unscroll-point' and 'unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))
