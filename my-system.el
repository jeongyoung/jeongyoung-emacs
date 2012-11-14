
;; (require 'emacs-type)

;; (setq make-backup-files nil)		;do not make backup files


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

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  ;(setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )
	 
	
	

	 