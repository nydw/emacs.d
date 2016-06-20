(require 'sr-speedbar)

(make-face 'speedbar-face)
;(set-face-font 'speedbar-face "Inconsolata-12")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

;(with-current-buffer sr-speedbar-buffer-name
;   (setq window-size-fixed 'width))

(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-width 30)
;(setq sr-speedbar-right-side nil) 

(global-set-key (kbd "<f5>") (lambda()
                               (interactive)
                               (sr-speedbar-toggle)))


























(provide 'speedbar-init)

;;; sr-speedbar.el ends here
