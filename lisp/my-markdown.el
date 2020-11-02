;; markdown mode [2012-11-15]

;; (autoload 'markdown-mode "markdown-mode.el" 
;;   "Major mode for editing Markdown files" t) 
;;
;; 2013-12-07 comment out because replace markdown 2.0 elpa package 


;; 확장자에 따른  markdown-mode를 자동으로 설정한다.
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("README$" . markdown-mode))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a /Applications/Marked.app %s" 
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

