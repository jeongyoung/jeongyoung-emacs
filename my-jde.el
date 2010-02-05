
(custom-set-variables
 '(jde-global-classpath (quote ("/System/Library/Frameworks/JavaVM.framework/Classes/" ".")))
 '(jde-jdk-registry (quote (("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0"))))
)

(add-to-list 'load-path "~/.emacs.d/plugins/jde/lisp")

(load "jde-autoload")
;(require 'jde)

(add-hook 'jde-mode-hook
          (lambda()
    (local-set-key [(control return)] 'jde-complete)
    (local-set-key [(shift return)] 'jde-complete-minibuf)
    (local-set-key [(meta return)] 'jde-complete-in-line)))