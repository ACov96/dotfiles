(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "4528fb576178303ee89888e8126449341d463001cb38abe0015541eb798d8a23" "2f99bc0390d837d5dd8f92c1df0472d4fce99f89e1dd536ae1a3ae9beeaa1b3e" "7d02557449e48c388b3265c1b48327bc33b2d7c59dde96f4262e523ede09b967" "36145e29308aa12ae0a4625d154a24f1778e177a3aad5a4622c305d35c2e093e" "e5c6caa4860b1ba51dc5ad335c0c2734ea650a6098dd9652a1ab3d9aa702e185" "cfc387d83d9f4806691121d8fa3d3943300d77de642a31e3282361bb3bc7b43f" "4f1c2c205d9df63d7c36cde9110e6d5e9965aec4b597a0e5381975d20e614cef" "35b4668e8858dba2d282534e5f221320caee7665ba8e434acc9d831481f21d4b" "9397f3ba34bdae8fec1183fc0b8aaf09599fab40af69ba58e167e3d2242327c0" "a4c04e631952c204c559c9fe1fd769d5f02545f68e9093c3d8ec9b4a226ab413" "cb9f879ffc4006bab31dba5a437194a0b1b922663ea31ee046a5950c0d6ca459" default)))
 '(exec-path
   (quote
    ("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/games" "/usr/games" "/usr/lib/emacs/25.1/x86_64-linux-gnu")))
 '(fci-rule-color "#383838")
 '(focus-dimness -5)
 '(helm-always-two-windows nil)
 '(helm-mode t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(js2-strict-trailing-comma-warning nil)
 '(linum-format " %7i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.1)
 '(nyan-bar-length 16)
 '(nyan-mode t)
 '(nyan-wavy-trail t)
 '(package-selected-packages
   (quote
    (dockerfile-mode docker fish-mode exec-path-from-shell json-mode column-enforce-mode babel evil-tutor django-mode jsx-mode markdown-preview-mode markdown-mode bison-mode zoom vue-mode mustache-mode use-package company projectile js2-mode smex w3m helm focus workgroups2 workgroups direx elscreen-multi-term fireplace beacon minimap nyan-mode web-mode flycheck tabbar git multi-term powerline restclient auto-complete base16-theme darkokai-theme color-theme-sanityinc-tomorrow doom-themes evil ng2-mode magit virtualenvwrapper neotree elscreen w3 moe-theme conkeror-minor-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;MELPA config
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(load-theme 'darkokai t)

;; hide menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; autocomplete braces, brackets, and parentheses
(electric-pair-mode 1)

;; collapse functions
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; match paren
(show-paren-mode 1)
(setq show-paren-delay 0)

;; resize windows faster
(defun win-resize-top-or-bot ()
    "Figure out if the current window is on top, bottom or in the
middle"
    (let* ((win-edges (window-edges))
	   (this-window-y-min (nth 1 win-edges))
	   (this-window-y-max (nth 3 win-edges))
	   (fr-height (frame-height)))
      (cond
       ((eq 0 this-window-y-min) "top")
       ((eq (- fr-height 1) this-window-y-max) "bot")
       (t "mid"))))

(defun win-resize-left-or-right ()
    "Figure out if the current window is to the left, right or in the
middle"
    (let* ((win-edges (window-edges))
	   (this-window-x-min (nth 0 win-edges))
	   (this-window-x-max (nth 2 win-edges))
	   (fr-width (frame-width)))
      (cond
       ((eq 0 this-window-x-min) "left")
       ((eq (+ fr-width 4) this-window-x-max) "right")
       (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;;linum-mode config (best to leave this at the end of the .emacs file)
(require 'hl-line)

(defface my-linum-hl
  `((t :inherit linum :background ,(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
			     (count-lines (point-min) (point-max))))))
	 (format (concat "%" (number-to-string width) "d \u2502")))
    (setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-format)

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
	      (if (eq line-number my-linum-current-line-number)
		  'my-linum-hl
		'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)

;; web-mode stuff
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-enable-current-element-highlight t)

;; js2-mode stuff
(require 'js2-mode)
(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))

(add-to-list 'load-path "~/.emacs.d/elpa/neotree-20170522.758")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; hide/show mode
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key [f4] 'hs-toggle-hiding)

;;(add-hook 'html-mode-hook 'linum-mode)
;; virtualenv stuff
(add-to-list 'load-path "~/.emacs.d/elpa/virtualenvwrapper-20161002.1515")
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)


;; custom eshell prompt
(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(setq eshell-prompt-function
      (lambda ()
	(concat (abbreviate-file-name default-directory) " $ ")))

(setq eshell-cmpl-cycle-completions nil)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; disable linum on eshell
(add-hook 'eshell-mode-hook
	  '(lambda () (linum-mode -1)))

;; better visual eshell stuff
(setq eshell-visual-commands '("node" "bash" "tmux" "fish"))
(setq eshell-visual-subcommands '("git" "log" "show" "diff"))
(setq eshell-destroy-buffer-when-process-dies t)

;; reload last emacs session
;;(desktop-save-mode 1)

;; show time
(display-time-mode 1)

;; nyan mode
(nyan-mode 1)

;; beacon
(beacon-mode 1)

;; better window movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; auto refresh file after it changes on disk
(global-auto-revert-mode 1)

;; helm
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

(add-to-list 'display-buffer-alist
	     `(,(rx bos "*helm" (* not-newline) "*" eos)
	       (display-buffer-in-side-window)
	       (inhibit-same-window . t)
	       (window-height . 0.25)))

;; backup stuff
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(put 'dired-find-alternate-file 'disabled nil)

;; evil mode stuff
(evil-mode 1)
(setq evil-default-state 'emacs)
(evil-set-initial-state 'term-mode 'emacs)

;; org-mode stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)))


;; tabs stuff
(setq tab-width 2)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)

;; eslint/flycheck stuff
(require 'flycheck)
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
(add-to-list 'exec-path "/home/covinga/.nvm/versions/node/v10.4.0/bin/")
(setq shell-command-switch "-c")
(exec-path-from-shell-initialize)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; startup stuff
(find-file "~/notes.org")
(switch-to-buffer "notes.org")


;; no tabs
(setq-default indent-tabs-mode nil)
