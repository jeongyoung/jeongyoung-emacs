(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun javascript-custom-setup ()
	(moz-minor-mode 1))

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(add-hook 'js-mode-hook 'javascript-custom-setup)

(defvar moz-repl-host "10.10.0.2")


