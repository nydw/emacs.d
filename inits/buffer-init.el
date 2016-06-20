(require 'desktop)

(setq desktop-path (concat data-path "desktop"))
(setq desktop-dirname (concat data-path "desktop"))
(setq desktop-base-file-name ".emacs-desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
	  '(lambda ()
	     ;; desktop-remove clears desktop-dirname
	     (setq desktop-dirname-tmp desktop-dirname)
	     (desktop-remove)
	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))




(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)



(setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	        "\\)$"))
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


;; end buffer-init
(provide 'buffer-init)
