(require 'package)

;;  패키지 저장소 Marmalade 추가
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


(package-initialize)

; fetch the list of packages available 
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

