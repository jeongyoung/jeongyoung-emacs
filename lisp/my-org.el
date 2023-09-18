;;
;; org-mode
;;
;; install from macport 

(use-package org
;  :ensure org-plus-contrib  
  :ensure t
  :mode (("\\.org$" . org-mode))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c b" . org-switchb)
  :config
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-ellipsis "⤵")
  (setq org-log-done 'time)
  (setq org-agenda-files (list "~/org/todo.org"))
  (setq org-adapt-indentation t)
  (setq org-agenda-directory "~/org")
  (setq org-agenda-files (directory-files (expand-file-name org-agenda-directory) t "^.*\\.org$"))
;  (require 'ox-confluence)
  )


;; (setq org-agenda-directory "~/Dropbox/Org")
;; (setq org-agenda-files
;;       (directory-files (expand-file-name org-agenda-directory) t "^.*\\.org$"))


(use-package org-bullets
  :ensure t
  ;;:hook (org-mode . org-bullets-mode))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))


;; (use-package org-caldav
;;   :init
;;   (defun org-caldav-sync-at-close ()
;;     (org-caldav-sync)
;;     (save-some-buffers))
;;   (setq org-caldav-url "http://11.71.33.65:8090/dav.php/calendars/jeongyoung")
;;   (setq org-icalendar-include-todo t
;; 	org-caldav-sync-todo t)
;;   (setq org-caldav-calendars
;; 	'((:calendar-id "default"
;; 			:files ("~/org/desk.org")
;; 			:inbox "~/org/org-caldav-inbox.org")))
;;   (setq org-caldav-backup-file "~/org/org-caldav-backup.org")
;;   (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
;;   (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
;;   ;(add-hook 'kill-emacs-hook 'org-caldav-sync-at-close)
;;   )

;; caldav로부터 상세 description 은 제외되도록 처리
;; (defun org-caldav--insert-description (description)
;;   (when (< (length description) 0)
;;     (when org-caldav-description-blank-line-before (newline))
;;     (let ((beg (point)))
;;       (insert description)
;;       (org-indent-region beg (point)))
;;     (when org-caldav-description-blank-line-after (newline))
;;     (newline)))

(use-package calfw
  :ensure t
  :bind ("C-c f" . cfw:open-org-calendar)
  :config
  (use-package calfw-org :ensure t)
  (setq cfw:org-overwrite-default-keybinding t
	cfw:display-calendar-holidays nil
	calendar-week-start-day 1))


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

