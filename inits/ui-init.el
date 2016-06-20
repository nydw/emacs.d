(require 'linumx)

(defun linum-face-setting ()
  "Face settings for `linumx'."
  (custom-set-faces
    '(linum
       ((((background "black")):(foreground "cyan"))
        (t :foreground "gray" :background "black")))))

(linum-face-setting)
(global-linum-mode 1)

(require 'molokai-theme)
(setq molokai-theme t)

(require 'powerline)

(defface youth-active1 '((t (:foreground "grey20" :weight bold :inherit mode-line
                                             :background "#afd700" :inherit mode-line)))
         "Powerline face 5."
         :group 'powerline)

(defface youth-active2 '((t (:foreground "#87ffaf" :inherit mode-line
                                             :background "grey20" :inherit mode-line)))
         "Powerline face 6."
         :group 'powerline)

(defface youth-active3 '((t (:foreground "brightyellow" :weight bold :inherit mode-line
                                             :background "grey40" :inherit mode-line)))
         "Powerline face 7."
         :group 'powerline)

(defface youth-active4 '((t (:foreground "white" :inherit mode-line
                                             :background "grey40" :inherit mode-line)))
         "Powerline face 8."
         :group 'powerline)


;;;###autoload
(defun powerline-youth-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'youth-active1 'powerline-inactive1))
                          (face2 (if active 'youth-active2 'powerline-inactive2))
                          (face3 (if active 'youth-active3 'powerline-inactive3))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw " " face2 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size face1 'l))
                                     (powerline-raw " " face1 'l)
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info face1 'l))
                                     (powerline-raw " " face1 'l)
                                    ;(powerline-vc face3 'r)
                                     (powerline-raw "%f" face2 'l)
                                     (powerline-raw "%* " face2 'l)))
                                     ;(powerline-buffer-id nil 'l)
                                     ;(when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                     ;(powerline-raw erc-modified-channels-object face1 'l))
                                     ;(powerline-major-mode face1 'l)
                                     ;(powerline-process face1)
                                     ;(powerline-minor-modes face1 'l)
                                     ;(powerline-narrow face1 'l)
                                     ;(funcall separator-left face1 face2)

                                     (rhs (list (powerline-raw global-mode-string face2 'r)
                                                (when (and (boundp 'which-func-mode) which-func-mode)
                                                  (powerline-raw which-func-format face2 'r))
                                                (set-face-foreground 'which-func "yellow")
                                                (set-face-background 'which-func "black")
                                                (powerline-raw "%4l" face1 'l)
                                                (powerline-raw ":" face1 'l)
                                                (powerline-raw "%3c" face1 'r)
                                                (powerline-raw " " face3 'r)
                                                (powerline-raw "%6p" face3 'r)
                                                (when powerline-display-hud
                                                  (powerline-hud face2 face1)))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-youth-theme)

(require 'unicad)
(unicad-enable)

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
                              highlight-parentheses-mode
                              (lambda ()
                                (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode 1)
(setq hl-paren-colors '("Red" "Cyan" "Brown" "Orange" "Magenta" "Green" "purple" "Red"))
(set-face-foreground 'minibuffer-prompt "white")


(line-number-mode 1)
(column-number-mode 1)
(which-function-mode 1)



(provide 'ui-init)

