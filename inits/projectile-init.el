(require 'projectile)

;(add-hook 'ruby-mode-hook 'projectile-mode)

(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-known-projects-file (concat data-path "projectile-bookmarks.eld"))

(defun projectile-cgrep ()
  "Perform rgrep in the project."
  (interactive)
  (let ((search-regexp (if (and transient-mark-mode mark-active)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string (projectile-prepend-project-name "CodeGrep for: ") (thing-at-point 'symbol))))
        (root-dir (expand-file-name (projectile-project-root))))
    (require 'grep)
    ;; paths for find-grep should relative and without trailing /
    (let ((grep-find-ignored-directories nil)
          (grep-find-ignored-files nil))
      (grep-compute-defaults)
      (rgrep search-regexp "Makefile *.c *.cc *.cpp *.h *.hh *.go *.py *.el *.sh" root-dir))))



(provide 'projectile-init)

