

(require 'package)

; list the packages you want
(setq my-package-list '
			(autopair cider clojure-mode company ecb emacs-eclim ggtags highlight-parentheses jedi auto-complete epc ctable concurrent deferred magit git-rebase-mode git-commit-mode markdown-mode pkg-info epl pony-mode popup psvn virtualenvwrapper s dash w3m weblogger xml-rpc yasnippet zenburn-theme)
)

;;  패키지 저장소 Marmalade 추가
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; 설치된 패키지들 활성화
(package-initialize)


; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package my-package-list)
  (when (not (package-installed-p package))
    (package-install package)))


