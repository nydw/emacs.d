(require 'multiple-cursors)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat data-path "cursor-places"))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x C-<") 'mc/mark-all-like-this)





(provide 'cursor-init)
