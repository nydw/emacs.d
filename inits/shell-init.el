(require 'exec-shell)

(setq comint-prompt-read-only t)
(setq shell-command-completion-mode t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



(provide 'shell-init)
