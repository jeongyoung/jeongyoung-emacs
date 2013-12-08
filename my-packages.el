

(require 'package)

; list the packages you want
(setq package-list '(auto-complete popup yasnippet autopair highlight-parentheses jedi weblogger xml-rpc ecb ggtags jedi magit pony-mode psvn))


;;  ��Ű�� �����
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; ��ġ�� ��Ű���� Ȱ��ȭ
(package-initialize)


; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


