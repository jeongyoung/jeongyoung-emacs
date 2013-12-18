
; JDEE 설정
;
; JDEE 이전 버전에서는 emacs24버전에 내장된 cedet를 인식하지못해서 에러가 발생했으나
; JDEE 2.4.1버전에서는 내장되어있는 CEDET에서 동작됨으로 현재까지 별다른 설정은 필요없음.
; 

(add-to-list 'load-path (concat my-root-dir "/vendor/jdee-2.4.1/lisp")) ;jde
	
(autoload 'jde-mode "jde" "JDE mode" t)
(setq auto-mode-alist
			(append '(("\\.java\\'" . jde-mode)) auto-mode-alist))


(custom-set-variables
 '(jde-compile-option-encoding "UTF-8")
 '(jde-check-version-flag nil)
)

; jde-compile 파일을 수정해서 java 1.7 컴파일이 가능하도록 처리

;;
;; jde-int
;;
;; Import JDEE projects from Eclipse.

;; Usage:
;;
;;   M-x jde-int-find-and-import-projects "/path/to/workspace"

;(require 'jde-int)

(defun my-java-minor () 
	(progn
;		(gtags-mode t)
;		(glasses-mode t)
		(auto-complete-mode t)
;		(add-to-list 'ac-sources ac-source-gtags)
		(local-set-key [(control return)] 'jde-complete)
		(local-set-key [(shift return)] 'jde-complete-minibuf)
		(local-set-key [(meta return)] 'jde-complete-in-line)))

(add-hook 'jde-mode-hook 'my-java-minor)


