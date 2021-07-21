;; (require 'emacs-type)

;; (setq make-backup-files nil)		;do not make backup files

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t) 



;; (let ((type (emacs-type)))
;;   ;;mac
;;   (cond ((eq type 'emacs-mac-window)
	
;; 	 ; mac에서 command 를 meta로 사용
;; 	 (setq mac-command-modifier 'meta))

;; 	)
;;   ;;windows
;;   (cond ((eq type 'emacs-window)
;; 	 )
;; 	)
;; )


(setq default-directory "~/" )

;; 프레임 전환을 메타키와 방향키로 가능하도록
(windmove-default-keybindings 'meta)

(setq exec-path (append exec-path '("/usr/local/bin")) )

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  ;(setq mac-option-modifier 'alt)

  (setq mac-command-modifier 'meta)

  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;; exec-path
(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))



(defun toggle-fullscreen ()
	"Toggle full screen"
	(interactive)
	(set-frame-parameter
	 nil 'fullscreen
	 (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


(global-set-key [(meta return)] 'toggle-fullscreen)
	
	

	 
