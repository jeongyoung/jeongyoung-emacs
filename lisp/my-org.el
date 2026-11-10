;; my-org.el --- description -*- lexical-binding: t; -*-


;; install from macport 

(use-package org
  :ensure nil ; 내장버전을 사용
  :defer t
  :mode (("\\.org$" . org-mode))
  :bind
;;  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c b" . org-switchb)
  :init
  (setq org-directory (expand-file-name "org" (getenv "HOME"))
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-archive-location (expand-file-name "archive/%s_archive::" org-directory)
        org-ellipsis "⤵"
        org-log-done 'time
        org-adapt-indentation t
        org-agenda-directory org-directory
        org-agenda-files
        (when (file-directory-p org-agenda-directory)
          (directory-files org-agenda-directory t "\\.org\\'"))))

(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))

(defun my-org-insert-file-metadata ()
  "Expand the metadata snippet when visiting a new, empty Org file."
  (when (and buffer-file-name
             (derived-mode-p 'org-mode)
             (string-match-p "\\.org\\'" buffer-file-name)
             (not (file-exists-p buffer-file-name))
             (zerop (buffer-size)))
    (require 'yasnippet)
    (yas-minor-mode 1)
    (yas-expand-snippet (yas-lookup-snippet "Org file metadata" 'org-mode))))

(add-hook 'find-file-hook #'my-org-insert-file-metadata)

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/tasks.org" "Tasks")
         "* TASK %?\n  DUE TO: %^t\n  %i")
	
		("p" "Todo" entry (file+headline "~/org/todo.org" "Todos")
         "* TODO %?\n  DUE TO: %^t\n  %i")
	
        ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* %? :IDEA:\n  %U\n  %i")

        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %? :NOTE:\n  %U\n  %i")        
	
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\n  Created: %U\n  %i")))


(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))



(provide 'my-org)
;;; my-org.el ends here
