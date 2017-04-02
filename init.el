(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives 
	     '("melpa" . "https://melpa.org/packages/")
	     '("org"   . "http://orgmode.org/elpa/"))
(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/mnt/HDD/Dropbox/Apps/orgzly/calendars/university.org" "/mnt/HDD/Dropbox/Apps/orgzly/calendars/sports.org" "/mnt/HDD/Dropbox/Apps/orgzly/calendars/societies.org" "/mnt/HDD/Dropbox/Apps/orgzly/calendars/options.org" "/mnt/HDD/Dropbox/Apps/orgzly/calendars/gcal.org" "/mnt/HDD/Dropbox/Apps/orgzly/todo.org" "/mnt/HDD/Dropbox/Apps/orgzly/inbox.org" "/mnt/HDD/Dropbox/MinorRP/LOSCAR/report/report.org")))
 '(package-selected-packages
   (quote
    (edit-server gmail-message-mode haskell-mode yasnippet flycheck fontawesome writeroom-mode use-package telephone-line tangotango-theme rainbow-mode rainbow-delimiters powerline-evil polymode pandoc-mode org-ref org-plus-contrib org-pdfview org-pandoc org-gcal org-bullets markdown-mode ivy-bibtex golden-ratio evil-org evil-nerd-commenter evil-magit evil-escape ess dash-functional counsel beacon auto-complete))))
