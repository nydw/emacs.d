;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup eal nil
  "Package use `eval-after-load' technique."
  :prefix "eal-")

;;;###autoload
(defcustom eal-loadfile-mode-maps
  `(("cc-mode"         nil                    c-mode-base-map)
    ("cc-mode"         c-mode                 c-mode-map)
    ("cc-mode"         c++-mode               c++-mode-map)
    ("cc-mode"         java-mode              java-mode-map)
    ("cc-mode"         awk-mode               awk-mode-map)
    "lisp-mode"
    ("lisp-mode"       emacs-lisp-mode        emacs-lisp-mode-map)
    "help-mode"
    ("man"             Man-mode               Man-mode-map)
    "log-view"
    ("compile"         compilation-mode       compilation-mode-map)
    ("gud")
    ("lisp-mode"       lisp-interaction-mode  lisp-interaction-mode-map)
    "browse-kill-ring"
    ("simple"          completion-list-mode   completion-list-mode-map)
    ("inf-ruby"        inferior-ruby-mode     inferior-ruby-mode-map)
    "ruby-mode"
    ("cus-edit"        custom-mode            custom-mode-map)
    ("info"            Info-mode              Info-mode-map)
    ("psvn"            svn-log-edit-mode      svn-log-edit-mode-map)
    ("psvn"            svn-status-mode        svn-status-mode-map)
    ("psvn"            svn-info-mode          svn-info-mode-map)
    ("package"         package-menu-mode      package-menu-mode-map)
    "dired"
    "apropos"
    "emaci"
    ("psvn"            svn-log-view-mode      svn-log-view-mode-map)
    ("vc-svn"          vc-svn-log-view-mode   vc-svn-log-view-mode-map)
    ("log-view"        log-view-mode          log-view-mode-map)
    "diff-mode"
    ("sgml-mode"       html-mode              html-mode-map)
    "sgml-mode"
    "w3m"
    ("data-debug"      data-debug-mode)
    ("debug"           debugger-mode          debugger-mode-map)
    "text-mode"
    "color-theme"
    "woman"
    "doxymacs"
    "grep"
    "view"
    ("hi-lock"         hi-lock-mode           hi-lock-map)
    "autoconf"
    "tcl"
    "sgml-mode"
    "image-mode"
    "shell"
    "sql"
    "rhtml-mode"
    "senator"
    "org"
    "org-agenda"
    "python"
    "groovy-mode"
    "nxml-mode"
    "perl-mode"
    "artist"
    "calendar"
    "outline"
    "google-maps-static"
    "flymake"
    ("speedbar"        speedbar-mode          speedbar-key-map)
    ("speedbar"        speedbar-mode          speedbar-file-key-map)
    ("chart"           chart-mode             chart-map)
    ("recentf"         recentf-dialog-mode    recentf-dialog-mode-map)
    ("conf-mode"       conf-javaprop-mode     conf-javaprop-mode-map)
    ("conf-mode"       conf-space-mode        conf-space-mode-map)
    ("cua-base"        nil                    cua--rectangle-keymap)
    ("make-mode"       makefile-gmake-mode    makefile-gmake-mode-map)
    ("make-mode"       makefile-mode          makefile-mode-map)
    ("make-mode"       makefile-automake-mode makefile-automake-mode-map)
    ("sh-script"       sh-mode                sh-mode-map)
    ("auto-complete"   auto-complete-mode     ac-completing-map)
    ("auto-complete"   nil                    ac-mode-map)

    ("semantic-decoration-on-include" nil semantic-decoration-on-include-map)
    ("semantic-symref-list" semantic-symref-results-mode semantic-symref-results-mode-map))
  "*List used to find load file by mode or map.

Every element of list is or a list consisted by load file, mode and map,
or just one load file, or nil. If element is a list, and its last element is nil,
it will be ignored."
  :type 'alist
  :group 'eal)

;;;###autoload
(defun eal-eval-by-modes (modes fun)
  "Run `eval-after-load' on function FUN by MODES.

FUN will be called by `eval' with argument mode of MODES.
Example:
\(eal-eval-by-modes
 ac-modes
 (lambda (mode)
   (let ((mode-name (symbol-name mode)))
     (when (and (intern-soft mode-name) (intern-soft (concat mode-name \"-map\")))
       (define-key (symbol-value (am-intern mode-name \"-map\")) (kbd \"C-c a\") 'ac-start)))))"
  (if (listp modes)
      (eal-eval-by-symbols modes 1 fun)
    (eal-eval-by-symbol modes 1 fun)))

;;;###autoload
(defun eal-eval-by-maps (maps fun)
  "Run `eval-after-load' on function FUN by MAPS.

FUN will be call by `eval' with argument mode of MAPS."
  (if (listp maps)
      (eal-eval-by-symbols maps 2 fun)
    (eal-eval-by-symbol maps 2 fun)))

;;;###autoload
(defun eal-eval-by-symbols (symbols pos fun)
  "Run `eval-after-load' on function FUN by SYMBOLS.

FUN will be call by `eval' with argument mode of SYMBOLS. "
  (mapc
   `(lambda (symbol)
      (eal-eval-by-symbol symbol ,pos ,fun))
   symbols))

;;;###autoload
(defun eal-eval-by-symbol (symbol pos fun)
  "Run `eval-after-load' on function FUN by SYMBOL."
  (let ((file (eal-find-loadfile-by-symbol symbol pos))
        (form `(,fun ',symbol)))
    (if file
        (eval-after-load file form)
      (eval form))))

;;;###autoload
(defun eal-find-loadfile-by-mode (mode)
  "Find load file by mode MODE."
  (eal-find-loadfile-by-symbol mode 1))

;;;###autoload
(defun eal-find-loadfile-by-map (map)
  "Find load file by map MAP."
  (eal-find-loadfile-by-symbol map 2))

;;;###autoload
(defun eal-find-loadfile-by-symbol (symbol pos)
  "Find load file by symbol SYMBOL, its position is POS."
  (let* ((symbol-name (symbol-name symbol))
         (first
          (find-if
           (lambda (pair)
             (if (stringp pair)
                 (if (string= symbol-name (eal-get-name-by-loadfile pair pos))
                     pair
                   (let ((file (and (string-match "^\\(.+\\)-mode$" pair)
                                    (match-string 1 pair))))
                     (if file
                         (if (string= symbol-name (eal-get-name-by-loadfile file pos))
                             pair))))
               (if pair
                   (if (eq (nth pos pair) symbol)
                       (car pair)))))
           eal-loadfile-mode-maps)))
    (if (listp first) (car first) first)))

;;;###autoload
(defun eal-get-name-by-loadfile (file pos)
  "Get `symbol-name' by load file FILE and position POS."
  (concat file "-" (if (= pos 1) "mode" "mode-map")))

;;;###autoload
(defun eal-define-keys (keymaps key-defs)
  "Execute `define-key' on KEYMAPS by `eval-after-load' technique use arguments from element of list KEY-DEFS.

KEY-DEFS should be one list, every element of it is a list
whose first element is key like argument of `define-key', and second element is command
like argument of `define-key'."
  (eal-eval-by-maps
   keymaps
   `(lambda (keymap)
      (eal-define-keys-commonly (symbol-value keymap) ',key-defs))))

;;;###autoload
(defun eal-define-keys-commonly (keymap key-defs)
  "Execute `define-key' on KEYMAP use arguments from KEY-DEFS.

KEY-DEFS should be one list, every element of it is a list
whose first element is key like argument of `define-key', and second element is command
like argument of `define-key'."
   (dolist (key-def key-defs)
     (when key-def
       (define-key keymap (eval `(kbd ,(car key-def))) (nth 1 key-def)))))

;;;###autoload
(defun eal-define-key (keymap key def)
  "Execute `define-key' use arguments KEYMAP, KEY, DEF by `eval-after-load' technique.

*Note*: KEYMAP should be quoted, this is diference between argument of `define-key'."
  (eal-eval-by-maps
   keymap
   `(lambda (keymap)
      (define-key (symbol-value keymap) ,key ',def))))

(defun forward-to-word (arg)
  "Move forward until encountering 
  the beginning of a word. With argument,  
  do this that many times."
  (interactive "p")
  (or (re-search-forward (if (> arg 0) 
                           "\\W\\b" "\\b\\W") nil t arg)
      (goto-char (if (> arg 0) (point-max) (point-min)))))

(defun backward-to-word (arg)
  "Move backward until encountering 
  the end of a word. With argument,  
  do this that many times."
  (interactive "p")
  (forward-to-word (- arg)))

(defun am-forward-word-or-to-word ()
  "`forward-word' or `forward-to-word'.
  If after excute `forward-to-word', 
  current position is at next line, 
  then rollback and excute `forward-word'"
  (interactive)
  (let ((noo (line-number-at-pos)) no)
    (save-excursion
      (forward-to-word 1)
      (setq no (line-number-at-pos)))
    (if (> no noo)
      (forward-word)
      (forward-to-word 1))))

;;;###autoload
(defmacro am-with-temp-mode (mode &rest body)
  "Create a temporary buffer with mode MODE, 
  and evaluate BODY there like `progn'.
  See also `with-temp-buffer'."
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

;;;###autoload
(defun am-equal-ignore-case (str1 str2)
  "STR1 equal ignore case to STR2 or not."
  (string= (downcase str1) (downcase str2)))


(defvar switch-major-mode-last-mode nil)

(defun major-mode-heuristic (symbol)
  (and (fboundp symbol)
       (string-match ".*-mode$" (symbol-name symbol))))

(defun switch-major-mode (mode)
  "切换major mode"
  (interactive
    (let ((fn switch-major-mode-last-mode) val)
      (setq val
            (completing-read
              (if fn (format "切换major-mode为(缺省%s): " fn) 
                "切换major-mode为: ")
              obarray 'major-mode-heuristic 
              t nil nil (symbol-name fn)))
      (list (intern val))))
  (let ((last-mode major-mode))
    (funcall mode)
    (setq switch-major-mode-last-mode last-mode)))


(defun get-mode-name ()
  "显示`major-mode'及`mode-name'"
  (interactive)
  (message "major-mode为%s,  
           mode-name为%s" major-mode mode-name))


(defun switch-to-scratch ()
  "切换到*scratch*"
  (interactive) 
  (let ((buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer buffer)
    (unless (equal major-mode 'lisp-interaction-mode)
      (lisp-interaction-mode))))

;;横切变竖切，竖切边横切
(defun toggle-window-split ()
  "Vertical split shows more of each line,  horizontal split shows
  more lines. This code toggles between them. It only works for
  frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
             (if (= (car this-win-edges)
                    (car (window-edges (next-window))))
               'split-window-horizontally
               'split-window-vertically)))
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1))))))


;; 调节行间距
(setq-default line-spacing 2) 
(defun toggle-line-spacing () 
"Toggle line spacing between 1 and 5 pixels." 
(interactive) 
(if (eq line-spacing 2) 
(setq-default line-spacing 7) 
(setq-default line-spacing 2)))

(defun go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
  Typing `go-to-char-key' again will move forwad to the next Nth
  occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
         (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))


(defalias 'move-beginning-of-line 'beginning-of-line)
(defalias 'move-end-of-line       'end-of-line)


(defun am-add-hooks (hooks function &optional append local)
  "Call `add-hook' on hook list HOOKS 
  use arguments FUNCTION, APPEND, LOCAL.
  HOOKS can be one list or just a hook."
  (if (listp hooks)
    (mapc
      `(lambda (hook)
         (add-hook hook ',function append local))
      hooks)
    (add-hook hooks function append local)))

(defun am-intern (&rest strings)
  "`intern' use STRINGS."
  (intern
    (apply
      'concat
      (mapcar
        (lambda (element)
          (if (stringp element) element (symbol-name element)))
        strings))))

(defun am-variable-is-t (symbol)
  "Return SYMBOL's value is t or not."
  (and (boundp symbol) (symbol-value symbol)))

(defmacro am-def-active-fun (symbol &optional fun-name)
  "Make definition of function 
  judge variable is active or not."
  `(defun ,(if fun-name fun-name symbol) ()
     ,(concat "`" (symbol-name symbol) "' is t or not.")
     (am-variable-is-t ',symbol)))

(defun apply-args-list-to-fun (fun-list args-list)
  "Apply args list to function FUN-LIST.
  FUN-LIST can be a symbol, also can be a list whose element is a symbol."
  (let ((is-list (and (listp fun-list) (not (functionp fun-list)))))
    (dolist (args args-list)
      (if is-list
        (dolist (fun fun-list)
          (apply-args-to-fun fun args))
        (apply-args-to-fun fun-list args)))))

;;;###autoload
(defun apply-args-to-fun (fun args)
  "Apply args to function FUN."
  (if (listp args)
    (eval `(,fun ,@args))
    (eval `(,fun ,args))))


(defun loop-load-path (dir)
  "把DIR的所有子目录都加到`load-path'里面"
  (interactive)
  (let ((default-directory (concat dir "/")))
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;; Fix bug of `normal-top-level-add-subdirs-to-load-path'
;; which can not add directory which name end with ".elc?"
;; copy from emacs23 startup.el and modify it
(defun normal-top-level-add-subdirs-to-load-path ()
  "Add all subdirectories of current directory to `load-path'.
More precisely, this uses only the subdirectories whose names
start with letters or digits; it excludes any subdirectory named `RCS'
or `CVS', and any subdirectory that contains a file named `.nosearch'."
  (let (dirs
        attrs
        (pending (list default-directory)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (push (pop pending) dirs)
      (let* ((this-dir (car dirs))
             (contents (directory-files this-dir))
             (default-directory this-dir)
             (canonicalized (if (fboundp 'untranslated-canonical-name)
                                (untranslated-canonical-name this-dir))))
        ;; The Windows version doesn't report meaningful inode
        ;; numbers, so use the canonicalized absolute file name of the
        ;; directory instead.
        (setq attrs (or canonicalized
                        (nthcdr 10 (file-attributes this-dir))))
        (unless (member attrs normal-top-level-add-subdirs-inode-list)
          (push attrs normal-top-level-add-subdirs-inode-list)
          (dolist (file contents)
            ;; The lower-case variants of RCS and CVS are for DOS/Windows.
            (unless (member file '("." ".." "RCS" "CVS" "rcs" "cvs"))
              (when (and (string-match "\\`[[:alnum:]]" file)
                         ;; Avoid doing a `stat' when it isn't necessary
                         ;; because that can cause trouble when an NFS server
                         ;; is down.
                         (file-directory-p file))
                (let ((expanded (expand-file-name file)))
                  (unless (file-exists-p (expand-file-name ".nosearch"
                                                           expanded))
                    (setq pending (nconc pending (list expanded)))))))))))
    (if (equal window-system 'w32)
        (setq load-path (append (nreverse dirs) load-path))
      (normal-top-level-add-to-load-path (cdr (nreverse dirs))))))


 ;; Function to collect information of packages.
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defvar package-init-statistic nil "Package loading statistics")

;; attempt to load a feature/library, failing silently
(defun try-require (feature &optional click)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (let ((timestamp (current-time))
            (package (if (stringp feature) feature (symbol-name feature))))
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (if click
            (add-to-list 'package-init-statistic
                         (cons (if (stringp feature) feature (symbol-name feature))
                               (float-time (time-since timestamp)))))
        (message "Checking for library `%s'... Found, cost %.2f seconds"
                 feature (float-time (time-since timestamp))))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

; package-install
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; loop load inits/* lisps/*
(loop-load-path  lisps-path)
(loop-load-path  inits-path)

; auto save  path/filename
(setq auto-save-list-file-prefix data-path)
(setq backup-directory-alist `((".*" . , data-path)))
(setq auto-save-file-name-transforms `((".*" , data-path t)))

; workspace path
(setq default-directory "~/" )
;(setq command-line-default-directory "~/")

; startup
(menu-bar-mode -1)
(setq default-tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default c-basic-offset 4)
(setq whitespace-global-mode t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq-default indent-tabs-mode nil)

; utils
(require 'ascii)


(provide 'init)

;;; custom.el ends here
