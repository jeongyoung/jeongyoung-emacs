

(require 'package)

; list the packages you want
(setq my-package-list '
			(company)
)

;;  ��Ű�� ����� Marmalade �߰�
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)


;; ��ġ�� ��Ű���� Ȱ��ȭ
(package-initialize)


; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package my-package-list)
  (when (not (package-installed-p package))
    (package-install package)))


