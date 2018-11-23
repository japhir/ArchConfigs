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
    ("~/SurfDrive/PhD/programming/standardstats/clumpedefficiency.org" "~/org/calendars/uu-periods.org" "~/org/calendars/university.org" "~/org/calendars/sports.org" "~/org/calendars/societies.org" "~/org/calendars/outofoffice.org" "~/org/calendars/options.org" "~/org/calendars/meetings.org" "~/org/calendars/gcal.org" "~/org/calendars/fest.org" "~/org/calendars/balance.org" "~/org/calendars/253plus.org" "~/org/calendars/253.org" "~/org/lists.org" "~/org/clumpednotes.org" "~/org/todo.org" "~/org/inbox.org" "~/SurfDrive/PhD/programming/long-term-etf/long-term-etf.org" "/home/japhir/Dropbox/Apps/orgzly/tickler.org")))
 '(org-entities-user
   (quote
    (("d18O" "\\ce{\\delta^18O}" nil "&delta;<sup>18</sup>O" "δ18O" "δ18O" "δ¹⁸O"))))
 '(package-selected-packages
   (quote
    (json-mode pandoc-mode poly-R poly-markdown apropospriate-theme apropospriate ess xresources-theme frames-only-mode org-ref-pdf org-books org-pomodoro diff-hl lispy git-gutter writeroom-mode ivy-yasnippet yasnippet evil-numbers evil-indent-plus evil-commentary evil-snipe evil-easymotion webpaste use-package-chords telephone-line systemd stan-mode ranger rainbow-mode rainbow-delimiters polymode ox-tufte org-ref org-plus-contrib org-pdfview org-noter org-gcal org-bullets openwith mu4e-conversation mu4e-alert monokai-theme leuven-theme ivy-bibtex hl-todo gmail-message-mode fontawesome flycheck evil-org evil-nerd-commenter evil-mu4e evil-magit evil-leader evil-escape evil-collection edit-server diminish define-word counsel-projectile bug-hunter beacon auto-complete ace-window)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0 :foreground "deep sky blue" :weight bold)))))
