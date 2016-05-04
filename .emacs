;;; Emacs config file
(package-initialize nil)

;; interface layout settings
(setq inhibit-splash-screen t) ; no splash screen
(scroll-bar-mode -1)   ; turn off the scroll bar
(menu-bar-mode -1)     ; turn off the menu
(show-paren-mode 1)    ; turn on paren match highlighting
(setq mouse-wheel-progressive-speed nil) ; disable scroll acceleration
(electric-pair-mode 1) ; auto-insert matching bracket

;;; Load package databases
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize nil)
(when (not package-archive-contents) (package-refresh-contents))

(setq package-enable-at-startup nil)
(package-initialize)

;;; Load specific packages
(require 'ess-site) ; emacs speaks statistics, work with R etc.
(require 'evil)
(require 'evil-org)
(require 'evil-magit)

;; evil stuff
(global-evil-leader-mode)
(evil-mode 1)
(evil-set-initial-state 'calendar-mode 'emacs)

;; prevent emacs from ruining my git repo's
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Easy symbol insertion
; C-x 8 o = degree
(global-set-key (kbd "C-x 8 a") (lambda () (interactive) (insert "α")))
(global-set-key (kbd "C-x 8 b") (lambda () (interactive) (insert "β")))
(global-set-key (kbd "C-x 8 d") (lambda () (interactive) (insert "δ")))

;; wrap at column 80
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
	  '(lambda() (set-fill-column 80)))

;;; mode loading
;; load pandoc and markdown modes
(add-hook 'markdown-mode-hook 'pandoc-mode)
;; load AUCTeX-mode
(setq TeX-suto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;; org-mode settings
(require 'org)
(setq org-agenda-files
      '("/mnt/HDD/Dropbox/Apps/orgzly/todo.org"))
(setq org-refile-targets
      '((nil :maxlevel . 2)  ; refile in the current buffer
	(org-agenda-files :maxlevel . 2))) ; refile to todo.org
(setq org-log-done 'time)
(defvar org-gtd-file "/mnt/HDD/Dropbox/Apps/orgzly/todo.org")
(defvar org-in-file "/mnt/HDD/Dropbox/Apps/orgzly/inbox.org")
;; this function opens my todo-file
(defun gtd ()
  "Open the GTD file"
  (interactive)
  (find-file org-gtd-file))
(defun inb ()
  "Open the inbox file"
  (interactive)
  (find-file org-in-file))
(define-key global-map "\C-cg" 'gtd)
(define-key global-map "\C-ci" 'inb)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file "/mnt/HDD/Dropbox/Apps/orgzly/inbox.org")
	 "* %?\n %i\n %a")
	("j" "Journal" entry (file+datetree "/mnt/HDD/Dropbox/Apps/orgzly/journal.org")
	 "* %?\nEntered on %U\n %i\n %a")))
;; the todo-states of my gtd-system
(setq org-todo-keywords 
      '((sequence "NEXT(n)" "WAITING(w!/!)" "SCHEDULED(a)" "SOMEDAY(s!/!)" "|" 
		  "DONE(d)" "CANCELLED(c)")))
;; prettify the todo keywords
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "light yellow" :background "red" :weight bold))
	("WAITING" . (:background "yellow"))
	("SCHEDULED" . (:background "light slate blue"))
	("SOMEDAY" . (:background "deep sky blue"))
	("DONE" . (:foreground "green4" :background "pale green"))
	("CANCELLED" . (:foreground "dim gray" :background "gray"))))
;; pretty bullets
(require 'org-bullets)
(setq org-bullets-bullet-list
      '("◉" "◎" "⚫" "○" "►" "◇"))
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
;; this is the amazing "view interesting tasks" menu
(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
	("gh" "Home" tags-todo "@home/+NEXT|+WAITING")
	("gu" "University" tags-todo "@UU/+NEXT|+WAITING")
	("gs" "Stad" tags-todo "@stad/+NEXT|+WAITING")
	("gl" "Laboratory" tags-todo "@lab/+NEXT|+WAITING")
	("gc" "Computer" tags-todo "@computer/+NEXT|+WAITING")
	("gi" "Internet" tags-todo "@internet/+NEXT|+WAITING")
	("ge" "Email" tags-todo "@email/+NEXT|+WAITING")
	("gb" "Bellen" tags-todo "@bellen/+NEXT|+WAITING")
	("ga" "Agenda" tags-todo "@agenda/+NEXT|+WAITING")
	("U" "Work: all UU-contexts"
	 ((tags-todo "@UU/+NEXT|+WAITING")
	  (tags-todo "@lab/+NEXT|+WAITING")
	  (tags-todo "@computer/+NEXT|+WAITING")
	  (tags-todo "@internet/+NEXT|+WAITING")
	  (tags-todo "@email/+NEXT|+WAITING")
	  (tags-todo "@bellen/+NEXT|+WAITING")
	  (tags-todo "@agenda/+NEXT|+WAITING"))
	 nil)
	("H" "Home: all personal contexts" 
	 ((tags-todo "@home/+NEXT|+WAITING")
	  (tags-todo "@stad/+NEXT|+WAITING")
	  (tags-todo "@computer/+NEXT|+WAITING")
	  (tags-todo "@internet/+NEXT|+WAITING")
	  (tags-todo "@email/+NEXT|+WAITING")
	  (tags-todo "@bellen/+NEXT|+WAITING")
	  (tags-todo "@agenda/+NEXT|+WAITING"))
	 nil)
	("n" todo "NEXT" nil)
	("w" todo "WAITING" nil)
	("s" todo "SOMEDAY" nil)
	("d" "Agenda + Next actions" ((agenda) (todo "NEXT")))
	))
(setq org-tag-alist '(("@home" . ?h)("@UU" . ?u)("@stad" . ?s)("@lab" . ?l)
		      ("@computer" . ?c)("@internet" . ?i)("@email" . ?e)
		      ("@bellen" . ?b)("@agenda" . ?a)))
;; extra org settings
(setq org-return-follows-link t)
(setq org-hide-leading-stars t)
(setf org-tags-column -65)
(setf org-special-ctrl-a/e t)
(setq org-log-done t) ; add date-entry upon task completion
(setq org-deadline-warning-days 14)
(setq org-fontify-emphasized-text t) 
(setq org-fontify-done-headline t)
;(setq org-agenda-include-all-todo nil) ; maybe if I notice that it is slow
(setq org-directory "/mnt/HDD/Dropbox/Apps/orgzly/")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
;(setq org-adapt-indentation nil) ;?
;(setq org-agenda-prefix-format "  %-17:c%?-12t% s") ;?

(setq org-fontifywhole-heading-line t) ;; works better with theme

;; org-gcal settings
;(require 'org-gcal)
;(setq org-gcal-client-id "r487anihgu01dvo0nc7vohho6g@group.calendar.google.com"
;      org-gcal-client-secret "")

;; theme setting
(load-theme 'leuven t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" default)))
 '(package-selected-packages
   (quote
    (fill-column-indicator ess writeroom-mode column-marker markdown-mode pandoc-mode org-pandoc)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
