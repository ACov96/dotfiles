;;; landmark-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "landmark" "landmark.el" (23054 16894 84931
;;;;;;  691000))
;;; Generated autoloads from landmark.el

(defalias 'landmark-repeat 'landmark-test-run)

(autoload 'landmark-test-run "landmark" "\
Run 100 Landmark games, each time saving the weights from the previous game.

\(fn)" t nil)

(autoload 'landmark "landmark" "\
Start or resume an Landmark game.
If a game is in progress, this command allows you to resume it.
Here is the relation between prefix args and game options:

prefix arg | robot is auto-started | weights are saved from last game
---------------------------------------------------------------------
none / 1   | yes                   | no
       2   | yes                   | yes
       3   | no                    | yes
       4   | no                    | no

You start by moving to a square and typing \\[landmark-start-robot],
if you did not use a prefix arg to ask for automatic start.
Use \\[describe-mode] for more info.

\(fn PARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; landmark-autoloads.el ends here
