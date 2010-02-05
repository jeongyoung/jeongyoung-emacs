;;
;;python mode
;; ~/.emacs.d/plugins/python-mode.el
(load "python-mode" nil t)
(autoload 'python-mode "python-mode" "Python editing mode." t)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode) 
				      interpreter-mode-alist))
(autoload 'python-mode "python" "Python editing mode." t)

(add-hook 'python-mode-hook
	    (lambda ()
	           (set (make-variable-buffer-local 'beginning-of-defun-function)
			  'py-beginning-of-def-or-class)
		        (setq outline-regexp "def\\|class ")
			     (eldoc-mode 1)))

;;ipython setting
(setq ipython-command "/opt/local/bin/ipython2.6")
(require 'ipython)
(setq py-python-command-args '("-colors" "NoColor"))

(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")


;;
;;pylookup
;;

;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup

(setq pylookup-dir "~/.emacs.d/pylookup")
(add-to-list 'load-path pylookup-dir)


;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in th Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(global-set-key "\C-ch" 'pylookup-lookup)


;;
;; django
;;

(defun django-shell (&optional argprompt)
  (interactive "P")
  ;; Set the default shell if not already set
  (labels ((read-django-project-dir 
        (prompt dir)
        (let* ((dir (read-directory-name prompt dir))
               (manage (expand-file-name (concat dir "manage.py"))))
          (if (file-exists-p manage)
              (expand-file-name dir)
            (progn
              (message "%s is not a Django project directory" manage)
              (sleep-for .5)
              (read-django-project-dir prompt dir))))))
(let* ((dir (read-django-project-dir 
             "project directory: " 
             default-directory))
       (project-name (first 
                      (remove-if (lambda (s) (or (string= "src" s) (string= "" s))) 
                                 (reverse (split-string dir "/")))))
       (buffer-name (format "django-%s" project-name))
       (manage (concat dir "manage.py")))
  (cd dir)
  (if (not (equal (buffer-name) buffer-name))
      (switch-to-buffer-other-window
       (apply 'make-comint buffer-name manage nil '("shell")))
    (apply 'make-comint buffer-name manage nil '("shell")))
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp (concat py-shell-input-prompt-1-regexp "\\|"
                                     py-shell-input-prompt-2-regexp "\\|"
                                     "^([Pp]db) "))
  (add-hook 'comint-output-filter-functions
            'py-comint-output-filter-function)
  ;; pdbtrack

  (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
  (setq py-pdbtrack-do-tracking-p t)
  (set-syntax-table py-mode-syntax-table)
  (use-local-map py-shell-map)
  (run-hooks 'py-shell-hook))))

;;django-mode
(load "django-mode.el")