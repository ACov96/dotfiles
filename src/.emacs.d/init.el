(require 'org)

(org-babel-load-file
 (expand-file-name "config.org"
		   (file-name-concat "~" ".emacs.d")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875"
     "6fc9e40b4375d9d8d0d9521505849ab4d04220ed470db0b78b700230da0a86c1"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
