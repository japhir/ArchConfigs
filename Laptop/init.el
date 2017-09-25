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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "next" :query "tag:next")
     (:name "waiting" :query "tag:waiting")
     (:name "survival" :query "tag:survival"))))
 '(org-agenda-files
   (quote
    ("~/Nextcloud/PhD/clumpednotes.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/uu-periods.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/sports.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/options.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/university.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/gcal.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/calendars/societies.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/lists.org" "~/.dropbox-personal/Dropbox/MRP/Report/Report.org" "~/.dropbox-personal/Dropbox/MinorRP/LOSCAR/report/report.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/inbox.org" "~/.dropbox-personal/Dropbox/Apps/orgzly/todo.org")))
 '(package-selected-packages
   (quote
    (systemd mu4e-alert mu4e org-gcal matlab-mode ranger smtpmail-multi notmuch monokai-theme monokai smooth-scrolling hl-todo auctex-latexmk latex-math-preview latex-pretty-symbols latex-preview-pane column-marker ag fontawesome latex-extra outline-magic golden-ratio writeroom-mode use-package telephone-line rainbow-mode rainbow-delimiters polymode pandoc-mode org-ref org-plus-contrib org-pdfview org-pandoc org-bullets markdown-mode ivy-bibtex evil-org evil-nerd-commenter evil-magit evil-escape ess dash-functional counsel beacon auto-complete)))
 '(smtpmail-smtp-server "solismail.uu.nl")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
