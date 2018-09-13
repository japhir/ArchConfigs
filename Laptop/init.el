(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org"   . "http://orgmode.org/elpa/"))
(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Dropbox/Apps/orgzly/clumpednotes.org" "~/Dropbox/Apps/orgzly/inbox.org" "~/Dropbox/Apps/orgzly/todo.org" "~/Dropbox/Apps/orgzly/calendars/gcal.org" "~/Dropbox/Apps/orgzly/calendars/options.org" "~/Dropbox/Apps/orgzly/calendars/societies.org" "~/Dropbox/Apps/orgzly/calendars/sports.org" "~/Dropbox/Apps/orgzly/calendars/university.org" "~/Dropbox/Apps/orgzly/calendars/fest.org" "~/Dropbox/Apps/orgzly/calendars/253plus.org" "~/Dropbox/Apps/orgzly/calendars/253.org" "~/Dropbox/Apps/orgzly/calendars/balance.org" "~/Dropbox/Apps/orgzly/calendars/meetings.org" "~/Dropbox/Apps/orgzly/calendars/outofoffice.org" "~/Dropbox/Apps/orgzly/calendars/uu-periods.org")))
 '(package-selected-packages
   (quote
    (evil-indent-plus evil-commentary evil-snipe evil-easymotion webpaste use-package-chords telephone-line systemd stan-mode ranger rainbow-mode rainbow-delimiters polymode ox-tufte org-ref org-plus-contrib org-pdfview org-noter org-gcal org-bullets openwith mu4e-conversation mu4e-alert monokai-theme leuven-theme ivy-bibtex hl-todo gmail-message-mode fontawesome flycheck evil-org evil-nerd-commenter evil-mu4e evil-magit evil-leader evil-escape evil-collection edit-server diminish define-word counsel-projectile bug-hunter beacon auto-complete ace-window)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
