(require 'smex)   ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay

(setq smex-save-file (concat data-path "smex-items"))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'smex-init)
