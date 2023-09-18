;; helm

(use-package helm
  :ensure t)
  ;; :init
  ;; (require 'helm-config))

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t)

(helm-mode 1)

(global-set-key (kbd "C-x b") 'helm-buffers-list)

(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-s") 'helm-occur)

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)


; wsl-copy
(defun wsl-copy (start end)
  (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
          (shell-command (concat "echo '" text "' | clip.exe")))))


;;wsl-paste
(defun wsl-paste ()
  (interactive)
  (let ((clipboard
	 (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove windows ^M characters
    (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
    (insert clipboard)))

(global-set-key (kbd "C-c C-c") 'wsl-copy)
(global-set-key (kbd "C-c C-v") 'wsl-paste)






