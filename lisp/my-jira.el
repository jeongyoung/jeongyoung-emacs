;;; my-jira.el --- Deferred Jira configuration -*- lexical-binding: t; -*-

(defconst my-jira-host "")
(defconst my-jira-user "")

(use-package jira
  :ensure t
  :commands (jira-issues jira-tempo)
  :init
  (setq jira-base-url (concat "https://" my-jira-host)
        jira-username my-jira-user
        jira-token nil
        jira-debug nil
        jira-api-version 3
        jira-issues-max-results 60
        jira-issues-table-fields
        '(:key :issue-type-name :status-name :assignee-name
               :progress-percent :work-ratio :created :summary)))

(defun my/get-jira-token ()
  "Return the Jira token from `auth-source', never from this file."
  (require 'auth-source)
  (when-let* ((auth (car (auth-source-search
                          :host my-jira-host
                          :user my-jira-user
                          :require '(:secret)
                          :max 1)))
              (secret (plist-get auth :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun my/jira-cloud-auth-header ()
  "Return a Basic Authorization header using the `auth-source' token."
  (let ((token (my/get-jira-token)))
    (unless (and (stringp token) (not (string= token "")))
      (user-error "No Jira token found in auth-source for %s" my-jira-host))
    (concat "Basic "
            (base64-encode-string (concat my-jira-user ":" token) t))))

(defun my/browse-url-windows (url &rest _)
  "Open URL through WSL's Windows browser bridge."
  (start-process "wslview" nil "wslview" url))

(when (and (getenv "WSL_DISTRO_NAME") (executable-find "wslview"))
  (add-to-list 'browse-url-handlers
               '("atlassian\\.net" . my/browse-url-windows)))

;; URL debugging can include sensitive request information in diagnostic
;; buffers, so leave it disabled for authenticated API calls.
(setq url-debug nil
      url-show-status t)

(use-package org-jira
  :ensure t
  :commands (org-jira-mode
             org-jira-get-issue-list
             org-jira-get-issues
             org-jira-get-issues-from-custom-jql
             org-jira-browse-issue)
  :init
  (setq jiralib-url (concat "https://" my-jira-host)
        org-jira-working-dir (expand-file-name "org/jira" (getenv "HOME"))
        org-jira-default-jql
        (concat
         "project = 'WIP' "
         "AND type IN (CR, SR) "
         "AND status NOT IN (취소됨, SR이관됨, CR이관됨, 완료됨) "
         "AND createdDate >= 2026-01-01 "
         "AND ('시스템(자산)' = "
         "'ari:cloud:cmdb::object/18b2ca4e-570e-4cd2-8e94-0de7eae3caef/12129' OR "
         "'시스템(자산)' = "
         "'ari:cloud:cmdb::object/18b2ca4e-570e-4cd2-8e94-0de7eae3caef/12297' OR "
         "'시스템(자산)' = "
         "'ari:cloud:cmdb::object/18b2ca4e-570e-4cd2-8e94-0de7eae3caef/102463' OR "
         "'시스템(자산)' = "
         "'ari:cloud:cmdb::object/18b2ca4e-570e-4cd2-8e94-0de7eae3caef/12254') "
         "AND assignee = currentUser() ORDER BY created DESC")
        org-jira-custom-jqls
        `((:jql ,(concat
                  "project = 'WIP' "
                  "AND type IN (CR, SR) "
                  "AND status NOT IN (취소됨, SR이관됨, CR이관됨) "
                  "AND createdDate >= 2026-01-01 "
                  "AND ('시스템(자산)' = "
                  "'ari:cloud:cmdb::object/18b2ca4e-570e-4cd2-8e94-0de7eae3caef/12129') "
                  "ORDER BY created DESC")
           :limit 500
           :filename "wmall-2.0")
          (:jql ,(concat
                  "project = 'WIP' "
                  "AND type IN (CR, SR) "
                  "AND status NOT IN (취소됨, SR이관됨, CR이관됨) "
                  "AND createdDate >= 2026-01-01 "
                  "AND ('시스템(자산)' = "
                  "'ari:cloud:cmdb::object/18b2ca4e-570e-4cd2-8e94-0de7eae3caef/12297') "
                  "ORDER BY created DESC")
           :limit 500
           :filename "wmall-1.0")))
  :config
  ;; Keep C-c c available for org-capture in Jira buffers.
  (define-key org-jira-entry-mode-map (kbd "C-c c") nil)
  (define-key org-jira-entry-mode-map (kbd "C-c j c") #'org-jira-add-comment)
  (define-key org-jira-entry-mode-map (kbd "C-c j u") #'org-jira-update-comment))

(provide 'my-jira)
;;; my-jira.el ends here
