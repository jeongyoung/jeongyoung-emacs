;;
;; CEDET
;; 
(load-file (concat my-root-dir "/vendor/cedet/common/cedet.el"))

(global-ede-mode 1)
(require 'semantic-ia)
(require 'semanticdb)
(global-semanticdb-minor-mode 1) 

