
(setq delete-by-moving-to-trash t)
;(add-to-list 'ido-ignore-files "\\.DS_Store")
;(setq mac-command-modifier 'meta)


(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))




