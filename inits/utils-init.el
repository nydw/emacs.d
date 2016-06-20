(require 'taglist)
(require 'guide-key)
(require 'smartscan)
(require 'buffer-move)
(require 'switch-window)
(require 'browse-kill-ring)
(require 'thing-edit-extension)


(setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "g" "z" "C-h"))
(setq guide-key/idle-delay 0.1)
(guide-key-mode 1)

;; -----set key-------

;; switch window
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-x o") 'switch-window)

;; set mark
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

; -----alias func-----

;; taglist
(defalias 'tag 'taglist)

;; browse kill ring
(defalias 'bkring 'browse-kill-ring)

;; symbol repalce
(smartscan-mode 1)
(defalias 'syr 'smartscan-symbol-replace)


;; (define-key global-map (kbd "C-c a") 'go-to-char)
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)


;; end utils-init
(provide 'utils-init)
