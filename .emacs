(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "4528fb576178303ee89888e8126449341d463001cb38abe0015541eb798d8a23" "2f99bc0390d837d5dd8f92c1df0472d4fce99f89e1dd536ae1a3ae9beeaa1b3e" "7d02557449e48c388b3265c1b48327bc33b2d7c59dde96f4262e523ede09b967" "36145e29308aa12ae0a4625d154a24f1778e177a3aad5a4622c305d35c2e093e" "e5c6caa4860b1ba51dc5ad335c0c2734ea650a6098dd9652a1ab3d9aa702e185" "cfc387d83d9f4806691121d8fa3d3943300d77de642a31e3282361bb3bc7b43f" "4f1c2c205d9df63d7c36cde9110e6d5e9965aec4b597a0e5381975d20e614cef" "35b4668e8858dba2d282534e5f221320caee7665ba8e434acc9d831481f21d4b" "9397f3ba34bdae8fec1183fc0b8aaf09599fab40af69ba58e167e3d2242327c0" "a4c04e631952c204c559c9fe1fd769d5f02545f68e9093c3d8ec9b4a226ab413" "cb9f879ffc4006bab31dba5a437194a0b1b922663ea31ee046a5950c0d6ca459" default)))
 '(fci-rule-color "#383838")
 '(inhibit-startup-screen t)
 '(linum-format " %7i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.1)
 '(nyan-bar-length 32)
 '(nyan-wavy-trail t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (zoom web-mode minimal-theme nyan-mode landmark pacmacs netrunner multi-term w3 tablist neotree moe-theme let-alist latex-preview-pane elscreen darkokai-theme conkeror-minor-mode async)))
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



;;moe-theme config
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/moe-theme-20170111.1838")
;; (add-to-list 'load-path "~/.emacs.d/elpa/moe-theme-20170111.1838")
;; (require 'moe-theme)
;; (setq moe-theme-highlight-buffer-id 1)
;; (moe-dark)
;; (moe-theme-set-color 'w/b)
(load-theme 'darkokai)

;; hide menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; elscreen stuff
;; (add-to-list 'load-path "~/.emacs.d/elpa/elscreen-20160613.251")
;; (elscreen-start)


;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

 (defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)
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

;; latex preview pane
(latex-preview-pane-enable)

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

(global-set-key [C-M-down] 'win-resize-minimize-vert)
(global-set-key [C-M-up] 'win-resize-enlarge-vert)
(global-set-key [C-M-left] 'win-resize-minimize-horiz)
(global-set-key [C-M-right] 'win-resize-enlarge-horiz)
(global-set-key [C-M-up] 'win-resize-enlarge-horiz)
(global-set-key [C-M-down] 'win-resize-minimize-horiz)
(global-set-key [C-M-left] 'win-resize-enlarge-vert)
(global-set-key [C-M-right] 'win-resize-minimize-vert)

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

;; typing of the emacs stuff
(autoload 'typing-of-emacs "typing" "The Typing of Emacs, a game." t)

;; nyan-mode
(nyan-mode 1)

;; hide/show mode
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key [f4] 'hs-toggle-hiding)

;; zoom
(custom-set-variables
 '(zoom-mode t))
(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(put 'dired-find-alternate-file 'disabled nil)
