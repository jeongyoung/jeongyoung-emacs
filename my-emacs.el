
(require 'emacs-type)

(setq make-backup-files nil)		;do not make backup files


(let ((type (emacs-type)))
  ;;mac
  (cond ((eq type 'emacs-mac-window)
	 (set-face-font 'default "NanumGothicCoding-14") 
	 ; mac에서 command 를 meta로 사용
	 (setq ns-command-modifier 'meta))

	)
  ;;windows
  (cond ((eq type 'emacs-window)
	 (set-face-font 'default "NanumGothicCoding-14"))
	 )
)
	 
	
	

	 