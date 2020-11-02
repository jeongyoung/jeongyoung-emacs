
;;
;; emacs eclim
;;
(require 'eclim)
(global-eclim-mode)


;; control eclimd
(require 'eclimd)

;Emacs-eclim tries its best to locate your Eclipse installation. 
;If you have Eclipse installed in a non-standard location (i.e. ~/opt/eclipse) you have two options:

(custom-set-variables
 '(eclim-eclipse-dirs '("~/Developer/eclipse-kepler")))



;Displaying compilation error messages in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)


(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

;Emacs-eclim can integrate with company-mode to provide pop-up dialogs for auto-completion. 
;To activate this, you need to add the following to your .emacs:

(require 'company)
(require 'company-emacs-eclim)

(company-emacs-eclim-setup)




(setq eclim-executable "~/Developer/eclipse-kepler/eclim")
(setq eclim-auto-save t)

(setq eclimd-default-workspace "~/Document/workspace_kepler")
(setq eclimd-wait-for-process nil)
