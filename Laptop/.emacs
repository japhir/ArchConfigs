;;; Emacs config file

;;; Load package databases
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(when (not package-archive-contents)
  (package-refresh-contents))

;;; emacs speaks statistics, work with R etc.
(require 'ess-site) 

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; prevent emacs from ruining my git repo's
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; interface layout settings
(setq inhibit-splash-screen t) ; no splash screen
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

;; Easy symbol insertion
; C-x 8 o = °, C-x 8 m = µ
(global-set-key (kbd "C-x 8 a") (lambda () (interactive) (insert "α")))
(global-set-key (kbd "C-x 8 b") (lambda () (interactive) (insert "β")))
(global-set-key (kbd "C-x 8 d") (lambda () (interactive) (insert "δ")))

;; wrap at column 80
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook
;	  '(lambda() (set-fill-column 80)))
; this is awesome when writing but very annoying when programming/working in r 

;;; mode loading
;; load pandoc and markdown modes
(add-hook 'markdown-mode-hook 'pandoc-mode)
;; load AUCTeX-mode
(setq TeX-suto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;; org-mode settings
(require 'org)

;; mobileorg
(setq org-mobile-inbox-for-pull "~/Dropbox/Apps/orgzly/inbox.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-force-id-on-agenda-items nil)
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

(setq org-agenda-files
      '("~/Dropbox/Apps/orgzly/todo.org"))
(setq org-refile-targets
      '((nil :maxlevel . 2)  ; refile within file
	(org-agenda-files :maxlevel . 2)))  ; refile to todo.org
(setq org-log-done 'time)
(defvar org-gtd-file "~/Dropbox/Apps/orgzly/todo.org")
;; this function opens my todo-file
(defun gtd ()
  "Open the GTD file"
  (interactive)
  (find-file org-gtd-file))
(define-key global-map "\C-cg" 'gtd)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Apps/orgzly/todo.org" "Inbox")
	 "* %?\n %i\n %a")
	("j" "Journal" entry (file+datetree "~/Dropbox/Apps/orgzly/journal.org")
	 "* %?\nEntered on %U\n %i\n %a")))
;; the todo-states of my gtd-system
(setq org-todo-keywords 
      '((sequence "NEXT(n)" "WAITING(w)" "SCHEDULED(a)" "SOMEDAY(s)" "|" 
		  "DONE(d)" "CANCELLED(c)")))
;; prettify the todo keywords
(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "light yellow" :background "red" :weight bold))
	("WAITING" . (:background "yellow"))
	("SCHEDULED" . (:background "light slate blue"))
	("SOMEDAY" . (:background "deep sky blue"))
	("DONE" . (:foreground "green4" :background "pale green"))
	("CANCELLED" . (:foreground "dim gray" :background "gray"))))
;; this is the amazing "view interesting tasks" menu
(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
	("gh" "Home" tags-todo "@home")
	("gu" "University" tags-todo "@UU")
	("gs" "Stad" tags-todo "@stad")
	("gl" "Laboratory" tags-todo "@lab")
	("gc" "Computer" tags-todo "@computer")
	("gi" "Internet" tags-todo "@internet")
	("ge" "Email" tags-todo "@email")
	("gb" "Bellen" tags-todo "@bellen")
	("ga" "Agenda" tags-todo "@agenda")
	("G" "GTD Block Agenda" ; everything in one buffer
	 ((tags-todo "@home")
	  (tags-todo "@UU")
	  (tags-todo "@stad")
	  (tags-todo "@lab")
	  (tags-todo "@computer")
	  (tags-todo "@internet")
	  (tags-todo "@email")
	  (tags-todo "@bellen")
	  (tags-todo "@agenda"))
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
;(setq org-agenda-include-all-todo nil); ??
(setq org-directory "~/Dropbox/Apps/orgzly/")
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
