;(autoload 'ggtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook
					(lambda ()
						(when (derived-mode-p 'c-mode 'c++-mode)
							(ggtags-mode 1))))
