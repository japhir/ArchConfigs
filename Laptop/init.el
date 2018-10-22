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
 '(custom-safe-themes
   (quote
    ("9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" default)))
 '(org-agenda-files
   (quote
    ("~/SurfDrive/PhD/programming/long-term-etf/long-term-etf.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/253.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/253plus.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/balance.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/fest.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/gcal.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/meetings.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/options.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/outofoffice.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/societies.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/sports.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/university.org" "/home/japhir/Dropbox/Apps/orgzly/calendars/uu-periods.org" "/home/japhir/Dropbox/Apps/orgzly/Getting Started with Orgzly.org" "/home/japhir/Dropbox/Apps/orgzly/LCMS.org" "/home/japhir/Dropbox/Apps/orgzly/archsetup.org" "/home/japhir/Dropbox/Apps/orgzly/birthdays.org" "/home/japhir/Dropbox/Apps/orgzly/clumpednotes.org" "/home/japhir/Dropbox/Apps/orgzly/gtdnotes.org" "/home/japhir/Dropbox/Apps/orgzly/inbox.org" "/home/japhir/Dropbox/Apps/orgzly/journal.org" "/home/japhir/Dropbox/Apps/orgzly/klimaatcommunicatie.org" "/home/japhir/Dropbox/Apps/orgzly/life.org" "/home/japhir/Dropbox/Apps/orgzly/lists.org" "/home/japhir/Dropbox/Apps/orgzly/mobin.org" "/home/japhir/Dropbox/Apps/orgzly/notes.org" "/home/japhir/Dropbox/Apps/orgzly/referencenotes.org" "/home/japhir/Dropbox/Apps/orgzly/someday.org" "/home/japhir/Dropbox/Apps/orgzly/someday_ticklethemind.org" "/home/japhir/Dropbox/Apps/orgzly/tickler.org" "/home/japhir/Dropbox/Apps/orgzly/todo.org" "/home/japhir/Dropbox/Apps/orgzly/triggerlist.org" "/home/japhir/Dropbox/Apps/orgzly/vocabulary.org" "/home/japhir/Dropbox/Apps/orgzly/wifiwachtwoorden.org")))
 '(org-entities-user
   (quote
    (("d18O" "\\ce{\\delta^18O}" nil "&delta;<sup>18</sup>O" "δ18O" "δ18O" "δ¹⁸O"))))
 '(package-selected-packages
   (quote
    (org-pomodoro diff-hl lispy git-gutter writeroom-mode ivy-yasnippet yasnippet evil-numbers evil-indent-plus evil-commentary evil-snipe evil-easymotion webpaste use-package-chords telephone-line systemd stan-mode ranger rainbow-mode rainbow-delimiters polymode ox-tufte org-ref org-plus-contrib org-pdfview org-noter org-gcal org-bullets openwith mu4e-conversation mu4e-alert monokai-theme leuven-theme ivy-bibtex hl-todo gmail-message-mode fontawesome flycheck evil-org evil-nerd-commenter evil-mu4e evil-magit evil-leader evil-escape evil-collection edit-server diminish define-word counsel-projectile bug-hunter beacon auto-complete ace-window)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0 :foreground "deep sky blue" :weight bold)))))
