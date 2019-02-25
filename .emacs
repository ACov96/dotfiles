;; Package manager config
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Detach the custom-file stuff from .emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Some visual niceties
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(show-paren-mode t)
(electric-pair-mode t)
(use-package darkokai-theme
  :ensure t
  :config (load-theme 'darkokai t))
(use-package smooth-scrolling
  :ensure t
  :init (setq smooth-scroll-margin 3)
  :config (smooth-scrolling-mode 1))

;; Mode Line stuffs
;(display-time 1)

;; Nyan Cat is the most important package of all
(use-package nyan-mode
  :ensure t
  :config ((lambda ()
	     (nyan-mode)
	     (nyan-start-animation)
	     (setq nyan-wavy-trail t))))

;; Helm
(use-package helm
  :ensure t
  :config ((lambda ()
	     (global-set-key (kbd "M-x") 'helm-M-x)
	     (global-set-key (kbd "C-x C-f") #'helm-find-files)
	     (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
	     (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
	     (define-key helm-map (kbd "C-z") #'helm-select-action)
	     (define-key helm-map (kbd "C-j") 'helm-next-line)
	     (define-key helm-map (kbd "C-k") 'helm-previous-line)
			 (add-to-list 'display-buffer-alist
										`(,(rx bos "*helm" (* not-newline) "*" eos)
											(display-buffer-in-side-window)
											(inhibit-same-window . t)
											(window-height . 0.25)))
			 (setq helm-autoresize-max-height 30
						 helm-autoresize-min-height 20))))

;; Auto-Complete
(use-package auto-complete
  :ensure t
  :config (progn (ac-config-default)
								 (setq ac-auto-start t)))

;; Line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d \u2502")

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)

;; Javascript
(use-package js2-mode
  :ensure t
  :config ((lambda ()
						 (setq tab-width 2)
						 (setq js-indent-level 2)
						 (setq js-switch-indent-offset 2))))

;; Flycheck (mostly for eslint)
(use-package flycheck
  :ensure t
  :config ((lambda () (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
	     (setq-default flycheck-disabled-checkers
			   (append flycheck-disabled-checkers
				   '(javascript-jshint)))
	     (flycheck-add-mode 'javascript-eslint 'js2-mode)
	     (setq-default flycheck-temp-prefix ".flycheck")
	     (setq-default flycheck-disabled-checkers
			   (append flycheck-disabled-checkers
				   '(json-jsonlist)))
	     ;; use local eslint from node_modules before global
	     ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
	     (defun my/use-eslint-from-node-modules ()
	       (let* ((root (locate-dominating-file
			     (or (buffer-file-name) default-directory)
			     "node_modules"))
		      (eslint (and root
				   (expand-file-name "node_modules/eslint/bin/eslint.js"
						     root))))
		 (when (and eslint (file-executable-p eslint))
		   (setq-local flycheck-javascript-eslint-executable eslint))))
	     (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
	     (add-to-list 'exec-path "/home/covinga/.nvm/versions/node/v11.2.0/bin/")
	     (setq shell-command-switch "-c")
	     (exec-path-from-shell-initialize)
	     (setq js2-mode-show-parse-errors nil)
	     (setq js2-mode-show-strict-warnings nil))))

;; Evil Leader (this needs to go before Evil setup)
(use-package evil-leader
  :ensure t
	:config ((lambda ()
						 (global-evil-leader-mode)
						 (evil-leader/set-leader "<SPC>"))))

;; Evil
(use-package evil
  :ensure t
  :config ((lambda ()
	     (evil-mode 1)
	     (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
	     (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
	     (define-key evil-insert-state-map (kbd "C-u") (lambda ()
							     (interactive)
							     (evil-delete (point-at-bol) (point)))))))

;; Elscreen (for tabs)
(use-package elscreen
  :ensure t
  :config (elscreen-start))

;; Setting up some macros
(evil-leader/set-key "f" #'helm-find-files)
(evil-leader/set-key "s" 'save-buffer)
(evil-leader/set-key "-" 'evil-window-split)
(evil-leader/set-key "/" 'evil-window-vsplit)
(evil-leader/set-key "<SPC>" 'helm-M-x)
(evil-leader/set-key "h" 'evil-window-left)
(evil-leader/set-key "j" 'evil-window-down)
(evil-leader/set-key "k" 'evil-window-up)
(evil-leader/set-key "l" 'evil-window-right)
(evil-leader/set-key "q" 'save-buffers-kill-emacs)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "d" 'kill-buffer)
(evil-leader/set-key "x" 'delete-window)
(evil-leader/set-key "c" 'elscreen-create)
(evil-leader/set-key "v" 'elscreen-kill)
(evil-leader/set-key "n" 'elscreen-next)
(evil-leader/set-key "p" 'elscreen-previous)
(evil-leader/set-key "0" '(lambda () (interactive) (elscreen-goto 0)))
(evil-leader/set-key "1" '(lambda () (interactive) (elscreen-goto 1)))
(evil-leader/set-key "2" '(lambda () (interactive) (elscreen-goto 2)))
(evil-leader/set-key "3" '(lambda () (interactive) (elscreen-goto 3)))
(evil-leader/set-key "4" '(lambda () (interactive) (elscreen-goto 4)))
(evil-leader/set-key "5" '(lambda () (interactive) (elscreen-goto 5)))
(evil-leader/set-key "6" '(lambda () (interactive) (elscreen-goto 6)))
(evil-leader/set-key "7" '(lambda () (interactive) (elscreen-goto 7)))
(evil-leader/set-key "8" '(lambda () (interactive) (elscreen-goto 8)))
(evil-leader/set-key "9" '(lambda () (interactive) (elscreen-goto 9)))

;; File backups
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
