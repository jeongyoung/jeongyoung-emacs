;;
;; 영한사전 펄스크립트 실행
;; ~/bin/dict
;; http://beonit2.tistory.com/entry/%EB%A6%AC%EB%88%85%EC%8A%A4%EC%9A%A9-daum-%EC%98%81%ED%95%9C-%EC%82%AC%EC%A0%84-%EC%8A%A4%ED%81%AC%EB%A6%BD%ED%8A%B8

(defun eng-dict (query)
  (interactive (list (read-from-minibuffer "eng>han : " (concat (thing-at-point 'symbol) "") nil nil)))
  (message (substring (shell-command-to-string (concat "~/bin/dict " query)) 0 -1))
  )
 
(defun search-eng-dict ()
  (interactive)
  (message (substring (shell-command-to-string (concat "~/bin/dict " (concat (thing-at-point 'symbol) ""))) 0 -1))
  )
 
(global-set-key [(f8)] 'search-eng-dict)
(global-set-key [(meta f8)] 'eng-dict)
