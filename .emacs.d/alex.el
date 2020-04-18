(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Evil Leader (this needs to go before Evil setup)
(use-package evil-leader
  :ensure t
  :config 
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f" #'helm-find-files
    "s" 'save-buffer
    "\\" 'split-window-below
    "/" 'split-window-horizontally
    "<SPC>" 'helm-M-x
    "m" 'delete-other-windows
    "B" 'balance-windows
    "q" 'save-buffers-kill-terminal
    "b" 'helm-buffers-list
    "d" 'kill-this-buffer
    "x" 'delete-window
    "n" 'centaur-tabs-forward
    "p" 'centaur-tabs-backward
    "+" 'text-scale-increase
    "-" 'text-scale-decrease
    "#" 'linum-relative-toggle
    "'" 'alex/eshell
    "w" 'ace-window
    "j" 'ace-jump-mode
    "i" 'info
    "c" 'alex/edit-config
    "r" 'alex/reload-config
    "*" 'alex/scratch
    ">" 'alex/transparency-increase
    "<" 'alex/transparency-decrease
    "T" 'alex/transparency-toggle))

;; Evil
(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-u" . evil-scroll-up)
              :map evil-visual-state-map
              ("C-u" . evil-scroll-up)) 
  :config 
  (evil-mode 1))


(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package evil-magit
  :ensure t
  :config
  (evil-leader/set-key "g" 'magit-status))

;; Helm
(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
   ("C-j" . helm-next-line)
   ("C-k" . helm-previous-line))
  :config 
  (use-package helm-descbinds
    :ensure t
    :bind (("C-h b" . helm-descbinds)
           ("C-h w" . helm-descbinds)))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.25)))
  (setq helm-autoresize-max-height 30
        helm-autoresize-min-height 20))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Window movement
(use-package ace-window
  :ensure t)

;; Movement in the buffer
(use-package ace-jump-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package vue-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "j") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "k") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package linum-relative
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'linum-relative-mode)
  (column-number-mode t)
  (add-hook 'vue-mode-hook 'linum-relative-mode))

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm
        projectile-idle-timer-hook t))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;; Javascript
(use-package js2-mode
  :ensure t
  :config 
  (use-package js2-refactor
    :ensure t)
  (add-hook 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq tab-width 2
        js-indent-level 2
        js-switch-indent-offset 2)
  (setq-default indent-tabs-mode nil))

;; Flycheck (mostly for eslint)
(use-package flycheck
  :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
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
  (setq js2-mode-show-strict-warnings nil))

;; Virtualenv
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
                              magit-blame-mode))) "Emacs")
      ((derived-mode-p 'prog-mode) "Editing")
      ((memq major-mode '(vue-mode)) "Editing")
      ((derived-mode-p 'dired-mode) "Dired")
      ((memq major-mode '(helpful-mode help-mode)) "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode)) "OrgMode")
      (t (centaur-tabs-get-group-name (current-buffer)))))))

(use-package auto-complete
  :ensure t
  :bind
  (:map ac-mode-map
        ("M-j" . ac-next)
        ("M-k" . ac-previous))
  :config
  (ac-config-default)
  (setq ac-auto-start t))

(setq scroll-step 1
      scroll-conservatively 10000
      inhibit-splash-screen t)
(show-paren-mode t)
(electric-pair-mode t)
(hl-line-mode t)

;; Nyan Cat is the most important package of all
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode)
  (nyan-start-animation)
  (setq nyan-wavy-trail t))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 3
        fci-rule-color "gray"
        fci-rule-column 80))

;; Transparency
(defvar alex/transparency-alpha 85)
(set-frame-parameter (selected-frame) 'alpha alex/transparency-alpha)
(add-to-list 'default-frame-alist (cons 'alpha alex/transparency-alpha))

(use-package xresources-theme
  :ensure t
  :config
  (load-theme 'xresources t)
  (add-hook 'after-make-frame-functions (lambda (frame) 
                                          (load-theme 'xresources t))))

(use-package all-the-icons
  :ensure t
  :config
  (let ((font-dest (cl-case window-system
                     (x  (concat (or (getenv "XDG_DATA_HOME")            ;; Default Linux install directories
                                     (concat (getenv "HOME") "/.local/share"))
                                 "/fonts/"))
                     (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                     (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
    (unless (file-exists-p (concat font-dest "all-the-icons.ttf"))
      (all-the-icons-install-fonts t))))

(use-package highlight-indentation
  :ensure t
  :hook prog-mode-hook)

(use-package powerline
  :disabled
  :ensure t
  :config
  (setq powerline-arrow-shape 'curve)
  (powerline-center-theme))

(use-package centered-window
  :ensure t
  :config)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package multiple-cursors
  :ensure t
  :bind
  (()))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts t
        dashboard-items '((recents . 5) (projects . 5))
        dashboard-center-content t))

;; Detach the custom-file stuff from .emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(use-package exec-path-from-shell
  :ensure t)

;; Don't display async shell when launching external commands
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defvar alex/tramp-limit 10000000)
(setq tramp-copy-size-limit alex/tramp-limit
      tramp-inline-compress-start-size alex/tramp-limit)

(setq sentence-end-double-space nil)

(fset 'yes-or-no-p 'y-or-n-p)

(auto-revert-mode t)

(use-package org
  :ensure t
  :config
  (use-package org-bullets
    :ensure t)

  (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook '(lambda () (auto-fill-mode t)))
  (add-hook 'org-mode-hook '(lambda () (yas-minor-mode t)))
  (add-hook 'org-mode-hook '(lambda () (centered-window-mode t)))
  (add-hook 'org-mode-hook '(lambda () (electric-pair-mode nil)))
  (add-hook 'org-mode-hook '(lambda () (company-mode nil)))
  (add-hook 'org-mode-hook '(lambda () (fci-mode t)))
  (add-hook 'org-mode-hook '(lambda () (org-bullets-mode 1)))

  ;; Use current window when editing src blocks
  (setq org-src-window-setup 'current-window)

  ;; Don't insert empty lines between org headings and items
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

  ;; Org mode source block language evaluation
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (js . t)
     (python . t)
     (shell . t)
     (latex . t)
     (java . t)
     (org . t))))

(unless (package-installed-p 'auctex)
  (package-refresh-contents)
  (package-install 'auctex))
(setq TeX-command-force "LaTeX")
(add-hook 'LaTeX-mode-hook 'hs-minor-mode)
(add-hook 'LaTeX-mode-hook 'linum-relative)
(add-hook 'after-save-hook (lambda ()
                             (when (equal major-mode 'latex-mode)
                               (TeX-command-master nil))))

(use-package gnus
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq user-mail-address "covinga@wwu.edu"
        user-full-name "Alex Covington"
        gnus-select-method '(nnnil)
        gnus-secondary-select-methods '((nnimap "wwu"
                                                (nnimap-address "outlook.office365.com")
                                                (nnimap-server-port 993)
                                                (nnimap-stream ssl)))
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-mime-display-multipart-related-as-mixed t
        gnus-auto-select-first nil
        gnus-summary-display-arrow nil
        message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 587))

(defun alex/eshell-prompt ()
  "My custom eshell prompt function."
  (let ((cwd (eshell/pwd)))
    (format "%s %s$ "
            (abbreviate-file-name cwd)
            (if (eq 0 (call-process "git" nil nil
                                    nil "-C" cwd "rev-parse"))
                (format "(%s) " (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
              ""))))

(setq eshell-prompt-function 'alex/eshell-prompt)

(defun alex/edit-config ()
  "Open my config file in a buffer for editing."
  (interactive)
  (find-file "~/.emacs.d/alex.org"))

(defun alex/reload-config ()
  "Reload my config file."
  (interactive)
  (org-babel-load-file "~/.emacs.d/alex.org"))

(defun alex/scratch ()
  "Switch to the *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer "*scratch*")))

(defun alex/eshell ()
  "Open eshell in a popup buffer"
  (interactive)
  (unless (string-equal (buffer-name) "*eshell*")
    (pop-to-buffer "*eshell*" t)
    (eshell)))

(defvar alex/dotfiles-dir "~/dotfiles")
(defun alex/commit-config ()
  "Commit my config to my dotfiles repository."
  (interactive)
  (copy-file "~/.emacs.d/alex.org"
             (concat (file-name-as-directory alex/dotfiles-dir) ".emacs.d/")
             t)
  (cd alex/dotfiles-dir)
  (shell-command "git add -u")
  (shell-command "git commit -m 'Updating config'")
  (magit-git-push (magit-get-current-branch)
                  (magit-get-upstream-branch)
                  (magit-push-arguments)))

(defun alex/transparency-increase ()
  "Increase the current frame's alpha value by +5"
  (interactive)
  (setq alex/transparency-alpha (min (+ alex/transparency-alpha 5)
                                     100))
  (set-frame-parameter (selected-frame) 'alpha alex/transparency-alpha))

(defun alex/transparency-decrease ()
  "Decrease the current frame's alpha value by -5"
  (interactive)
  (setq alex/transparency-alpha (max (- alex/transparency-alpha 5)
                                     0))
  (set-frame-parameter (selected-frame) 'alpha alex/transparency-alpha))

(defun alex/transparency-toggle ()
  "Toggles control of transparency between Emacs and the window manager."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha
                       (if (eq 100 (frame-parameter (selected-frame) 'alpha))
                           alex/transparency-alpha
                         100)))
