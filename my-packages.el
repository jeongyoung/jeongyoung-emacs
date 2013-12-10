

(require 'package)

; list the packages you want
(setq package-list '(auto-complete 
										 autopair 
										 concurrent 
										 ctable 
										 deferred 
										 epc 
										 highlight-parentheses 
										 weblogger 
										 xml-rpc 
										 ecb 
										 ggtags 
										 jedi 
										 magit 
										 pony-mode 
										 popup 
										 psvn 
										 yasnippet
										 flymake
										 ))

;;  패키지 저장소 Marmalade 추가
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
                                                 '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; 설치된 패키지들 활성화
(package-initialize)


; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


