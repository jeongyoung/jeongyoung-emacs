;;
;; org-mode
;;
;; install from macport 


;; (require 'org)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

;; (setq org-agenda-files (list "~/Dropbox/Org/work.org"
;; 			     "~/Dropbox/Org/home.org"
;; 			     "~/Dropbox/Org/project.org"
;; 			     "~/Dropbox/Org/emacs.org"
;; 			     "~/Dropbox/Org/OrgTutorial.org"))

;; (setq org-agenda-directory "~/Dropbox/Org")
;; (setq org-agenda-files
;;       (directory-files (expand-file-name org-agenda-directory) t "^.*\\.org$"))

;; (setq org-mobile-directory "~/Dropbox/MobileOrg")


;; ;(setq org-refile-targets (quote (("newgtd.org" :maxlevel . 1) ("someday.org" :level . 2))))


;; ;; (setq org-combined-agenda-icalendar-file
;; ;;       "~/Library/Calendars/OrgMode.ics")
;; ;; (add-hook 'org-after-save-iCalendar-file-hook
;; ;; 	  (lambda ()
;; ;; 	    (shell-command
;; ;; 	     "osascript -e 'tell application \"iCal\" to reload calendars'")))


;; ;;
;; ;; remember-mode 
;; ;;
;; (org-remember-insinuate)
;; (setq org-directory "~/Dropbox/Org/")
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
;; (global-set-key "\C-cr" 'org-remember)

;; (setq org-remember-templates
;;       '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
;; 	 "~/Dropbox/Org/newgtd.org" "Tasks"))
;; )

;; ;; (setq org-agenda-custom-commands
;; ;;       '(

;; ;; 	("P" "Projects"
;; ;; 	 ((tags "PROJECT")))

;; ;; 	("H" "Office and Home Lists"
;; ;; 	 ((agenda)
;; ;; 	  (tags - todo "OFFICE")
;; ;; 	  (tags - todo "HOME")
;; ;; 	  (tags - todo "COMPUTER")
;; ;; 	  (tags - todo "DVD")
;; ;; 	  (tags - todo "READING")))

;; ;; 	("D" "Daily Action List"
;; ;; 	 (
;; ;; 	  (agenda "" ((org-agenda-ndays 1)
;; ;; 		      (org-agenda-sorting-strategy
;; ;; 		       (quote ((agenda time-up priority-down tag-up) )))
;; ;; 		      (org-deadline-warning-days 0)
;; ;; 		      ))))


;; ;; 	)
;; ;;       )

;; (defun gtd ()
;;   (interactive)
;;   (find-file "~/Dropbox/Org/newgtd.org"))
;; (global-set-key (kbd "C-c g") 'gtd)

;; (custom-set-variables
;;  '(org-hide ((((background dark)) (:foreground "darkslateg"))))
;; )

;; ;; org-toodledo

;; ;; set this variables in ~/.emacs.private/...
;; ;; (setq org-toodledo-userid "")    
;; ;; (setq org-toodledo-password "")


;; (require 'org-toodledo)

;; ;; Useful key bindings for org-mode
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-unset-key "\C-o")
;;             (local-set-key "\C-od" 'org-toodledo-mark-task-deleted)
;;             (local-set-key "\C-os" 'org-toodledo-sync)
;;             )
;;           )
;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (local-unset-key "\C-o")
;;             (local-set-key "\C-od" 'org-toodledo-agenda-mark-task-deleted)
;;             )
;; 	  )	

