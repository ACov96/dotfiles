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
(setq scroll-step 1
      scroll-conservatively 10000)

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

;; Company mode for auto completion
(use-package company
  :ensure t
  :config
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode))

;; Line numbers
(use-package linum-relative
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'linum-relative-mode)
  (column-number-mode t)
  (add-hook 'vue-mode-hook 'linum-relative-mode)
  )

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)


;; Javascript
(use-package js2-mode
  :ensure t
  :config ((lambda ()
	     (add-hook 'auto-mode-alist '("\\.js\\'" . js2-mode))
	     (setq tab-width 2)
	     (setq js-indent-level 2)
	     (setq js-switch-indent-offset 2)
	     (setq-default indent-tabs-mode nil))))

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

;; Centaur tabs
(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode 1)
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*")
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((memq major-mode '(vue-mode))
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))

;; Multi-term
(use-package multi-term
	:ensure t)

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
(evil-leader/set-key "q" 'delete-frame)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "d" 'kill-this-buffer)
(evil-leader/set-key "x" 'delete-window)
(evil-leader/set-key "n" 'centaur-tabs-forward)
(evil-leader/set-key "p" 'centaur-tabs-backward)
(evil-leader/set-key "+" '(lambda () (interactive) (text-scale-increase)))
(evil-leader/set-key "-" '(lambda () (interactive) (text-scale-decrease)))
(evil-leader/set-key "#" 'linum-relative-toggle)

;; File backups
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; Which-Key Reminders
(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package go-mode
  :ensure t)

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package vue-mode
  :ensure t)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; Neotree and project directory structure
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  )

;; Code folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Distractions...
(use-package sudoku
  :ensure t)
(put 'dired-find-alternate-file 'disabled nil)

;; Latex stuff
(unless (package-installed-p 'auctex)
  (package-refresh-contents)
  (package-install 'auctex))
(setq TeX-command-force "LaTeX")
(add-hook 'LaTeX-mode-hook 'hs-minor-mode)
(add-hook 'LaTeX-mode-hook 'linum-relative)
(add-hook 'after-save-hook (lambda ()
                             (when (equal major-mode 'latex-mode)
                               (TeX-command-master nil))))

;; Don't display async shell when launching external commands
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)
(evil-leader/set-key "g" 'magit-status)

;; Virtualenv
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs"))
