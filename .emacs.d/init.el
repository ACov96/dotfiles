;; Package manager config
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Load the Org file
(require 'org)
(org-babel-load-file "~/.emacs.d/alex.org")
