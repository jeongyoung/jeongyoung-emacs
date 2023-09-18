;; custom js 를 해석해서 후보로 추천


;; elpa package ac-js2 , js2-mode 

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook 'ac-js2-mode)

;; add minior mode

;(add-hook 'js-mode-hook 'js2-minor-mode)


(setq ac-js2-evaluate-calls t)





;;Add any external Javascript files to the variable below. Make sure you have initialised ac-js2-evaluate-calls to t if you add any libraries.

;(setq ac-js2-external-libraries '("full/path/to/a-library.js"))



