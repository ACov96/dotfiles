;; Package manager config
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Load the Org file
(require 'org)
(if (and (file-exists-p "~/.emacs.d/alex.el")
         (file-newer-than-file-p "~/.emacs.d/alex.el" "~/.emacs.d/alex.org"))
    (load "~/.emacs.d/alex")
  (org-babel-load-file "~/.emacs.d/alex.org"))

(put 'dired-find-alternate-file 'disabled nil)
