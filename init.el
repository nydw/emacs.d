
(defconst data-path      "~/.data/emacs/" "save all data")
(defconst base-path      "~/.emacs.d/" "base config directory")
(defconst tools-path     (concat base-path "tools/") "dependent tools")
(defconst lisps-path     (concat base-path "lisps/") "standward plugin")
(defconst inits-path     (concat base-path "inits/") "plugin init config")

; load inits/init.el
(load (concat inits-path "init"))

(require 'ui-init)
(require 'avy-init)
(require 'evil-init)
(require 'smex-init)
(require 'helm-init)
(require 'buffer-init)
(require 'golang-init)
(require 'projectile-init)

;(require 'gtags-init)
;(require 'company-init)
;(require 'speedbar-init)
;

(require 'utils-init)








