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
    ("~/org/calendars/uu-periods.org" "~/org/calendars/university.org" "~/org/calendars/sports.org" "~/org/calendars/societies.org" "~/org/calendars/outofoffice.org" "~/org/calendars/options.org" "~/org/calendars/meetings.org" "~/org/calendars/gcal.org" "~/org/calendars/fest.org" "~/org/calendars/balance.org" "~/org/calendars/253plus.org" "~/org/calendars/253.org_archive" "~/org/calendars/253.org" "~/org/todo.org" "~/org/someday.org" "~/org/notes.org" "~/org/mobin.org" "~/org/lists.org" "~/org/journal.org" "~/org/inbox.org" "~/org/clumpednotes.org")))
 '(package-selected-packages
   (quote
    (weechat edit-server gmail-message-mode haskell-mode yasnippet flycheck fontawesome writeroom-mode use-package telephone-line tangotango-theme rainbow-mode rainbow-delimiters powerline-evil polymode pandoc-mode org-ref org-plus-contrib org-pdfview org-pandoc org-gcal org-bullets markdown-mode ivy-bibtex golden-ratio evil-org evil-nerd-commenter evil-magit evil-escape ess dash-functional counsel beacon auto-complete)))
 '(send-mail-function (quote mailclient-send-it)))
