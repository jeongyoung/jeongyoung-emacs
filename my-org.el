;;
;; org-mode
;;
;; install from macport 

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; (setq org-agenda-files (list "~/org/work.org"
;; 			     "~/org/home.org"
;; 			     "~/org/project.org"
;; 			     "~/org/emacs.org"
;; 			     "~/org/OrgTutorial.org"))

(setq org-agenda-directory "~/org/")
(setq org-agenda-files
      (directory-files (expand-file-name org-agenda-directory) t "^.*\\.org$"))

(setq org-mobile-directory "/Volumes/jeongyoung/org/")


;(setq org-refile-targets (quote (("newgtd.org" :maxlevel . 1) ("someday.org" :level . 2))))


;; (setq org-combined-agenda-icalendar-file
;;       "~/Library/Calendars/OrgMode.ics")
;; (add-hook 'org-after-save-iCalendar-file-hook
;; 	  (lambda ()
;; 	    (shell-command
;; 	     "osascript -e 'tell application \"iCal\" to reload calendars'")))


;;
;; remember-mode 
;;
(org-remember-insinuate)
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key "\C-cr" 'org-remember)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
	 "~/org/newgtd.org" "Tasks"))
)

;; (setq org-agenda-custom-commands
;;       '(

;; 	("P" "Projects"
;; 	 ((tags "PROJECT")))

;; 	("H" "Office and Home Lists"
;; 	 ((agenda)
;; 	  (tags - todo "OFFICE")
;; 	  (tags - todo "HOME")
;; 	  (tags - todo "COMPUTER")
;; 	  (tags - todo "DVD")
;; 	  (tags - todo "READING")))

;; 	("D" "Daily Action List"
;; 	 (
;; 	  (agenda "" ((org-agenda-ndays 1)
;; 		      (org-agenda-sorting-strategy
;; 		       (quote ((agenda time-up priority-down tag-up) )))
;; 		      (org-deadline-warning-days 0)
;; 		      ))))


;; 	)
;;       )

(defun gtd ()
  (interactive)
  (find-file "~/org/newgtd.org"))
(global-set-key (kbd "C-c g") 'gtd)

(custom-set-variables
 '(org-hide ((((background dark)) (:foreground "darkslateg"))))
)