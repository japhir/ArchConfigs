;;; Load package databases
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives 
	     '("melpa" . "https://melpa.org/packages/")
	     '("org" . "http://orgmode.org/elpa/"))
	   
(package-initialize)

;; bootstrap use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
;; (require 'diminish) ; TODO see what this is useful for

;; prevent emacs from ruining my git repo's
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; interface layout settings
(setq inhibit-splash-screen t) 
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting
(scroll-bar-mode -1)   ; turn off the scroll bar
(menu-bar-mode -1)     ; turn off the menu
(setq-default fill-column 79)
;(setq mouse-wheel-progressive-speed nil) ; disable scroll acceleration

;; Easy symbol insertion
; C-x 8 o = °, C-x 8 m = µ
(global-set-key (kbd "C-x 8 a") (lambda () (interactive) (insert "α")))
(global-set-key (kbd "C-x 8 b") (lambda () (interactive) (insert "β")))
(global-set-key (kbd "C-x 8 d") (lambda () (interactive) (insert "δ")))

;; my gtd and inbox files
(defvar org-gtd-file "~/Dropbox/Apps/orgzly/todo.org")
(defvar org-in-file "~/Dropbox/Apps/orgzly/inbox.org")
;; this function opens my todo-file
(defun open-gtd-file ()
  "Open the GTD file"
  (interactive)
  (find-file org-gtd-file))
(defun open-inbox-file ()
  "Open the inbox file"
  (interactive)
  (find-file org-in-file))
(define-key global-map "\C-cg" 'open-gtd-file)
(define-key global-map "\C-ci" 'open-inbox-file)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defalias 'yes-or-no-p 'y-or-n-p)
;; C-l clears the eshell buffer
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
	  '(lambda()
	     (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(use-package counsel
  :ensure t)
;; very nice search replacement
(use-package swiper
  :init (ivy-mode 1)
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  :bind
  ("\C-s" . swiper)
  ("C-c C-r" . ivy-resume)
  ("<f6>" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-load-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ;;("C-c g" . counsel-git) ;; conflicts with my view gtd file command
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate))
  ;;("C-S-o" . counsel-rhythmbox))
;; jump to next char
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))
;; emacs speaks statistics, work with R etc.
(use-package ess 
  :ensure t
  ; :config (setq ess-default-style 'RStudio)
  :commands R)
;; for orking with .Rmd files etc. 
(use-package polymode
  :ensure t
  :mode
    ;; MARKDOWN
    ("\\.md" . poly-markdown-mode)
    ;; R modes
    ("\\.Snw" . poly-noweb+r-mode)
    ("\\.Rnw" . poly-noweb+r-mode)
    ("\\.Rmd" . poly-markdown+r-mode))
(use-package matlab
  :init (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  :mode ("\\.m\\'" . matlab-mode)
  :interpreter "matlab"
  :config
  (setq matlab-indent-function t)
  (setq matlab-indent-function "matlab"))
;(use-package systemd ;; not sure why I have this...
;  :ensure t)
(use-package evil-nerd-commenter
  :ensure t)
(use-package evil-leader  ; default is \
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    ;;"\\" 'evilnc-comment-operator ; if you prefer backslash key
    ))
;; vim emulator
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  :bind (:map evil-normal-state-map
	      ("C-h" . evil-window-left)
	      ("C-j" . evil-window-down)
	      ("C-k" . evil-window-up)
	      ("C-l" . evil-window-right)))
(use-package evil-org
  :ensure t)
(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"))
(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status))
(use-package evil-magit
  :ensure t)
(use-package powerline-evil
  :ensure t
  :config (powerline-evil-vim-color-theme))
(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (global-auto-complete-mode t))
(use-package markdown-mode
  :ensure t)
(use-package pandoc-mode
  :defer 
  :init (add-hook 'markdown-mode-hook 'pandoc-mode))
(use-package tex
  :defer 
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

;; reference manager
(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography '("/home/japhir/Documents/References/MRP.bib" "/home/japhir/Documents/References/Minor Research Project.bib"))
  (setq bibtex-completion-library-path '("/home/japhir/Dropbox/MRP/References"))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-notes-path "/home/japhir/Dropbox/Apps/orgzly/referencenotes.org")
  ;; open bibtex links with zathura
  (setq bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "zathura" nil 0 nil fpath)))
  (setq bibtex-completion-browser-function 'browser-url-chromium)
  (setq bibtex-completion-format-citation-functions
	'((org-mode      . bibtex-completion-format-citation-cite)
	  (latex-mode    . bibtex-completion-format-citation-cite)
	  (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	  (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-cite-default-command "citep")
  ;; no before or after promts
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  ;; not fullscreen (not working)
  ;;(setq bibtex-completion-full-frame nil)
  :bind
  ("C-x c" . helm-bibtex))
(use-package org
  :ensure t
  :config
  (setq org-highlight-latex-and-related '(latex script entities))  
  ;; org settings for my todo system
  (setq org-agenda-files (list "~/Dropbox/Apps/orgzly/inbox.org"
			       "~/Dropbox/Apps/orgzly/todo.org"
			       "~/LOSCAR/Loscar-2.0.4/Notes.org"
			       "~/Dropbox/Apps/orgzly/calendars/IljaKocken.org"
			       "~/Dropbox/Apps/orgzly/calendars/marinesciences.org"
			       "~/Dropbox/Apps/orgzly/calendars/assistant.org"
			       "~/Dropbox/Apps/orgzly/calendars/ubv.org"
			       "~/Dropbox/Apps/orgzly/calendars/options.org"))
  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  ;;(setq org-completion-use-ido t)
  ;;(setq org-refile-targets
  ;;      '((nil :maxlevel . 3)  ; refile within file
  ;;	  (org-agenda-files :maxlevel . 2)))  ; refile to todo.org
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/Dropbox/Apps/orgzly/inbox.org")
	   "* %?\n %i\n %a")
	  ("j" "Journal" entry (file+datetree "~/Dropbox/Apps/orgzly/journal.org")
	   "* %?\nEntered on %U\n %i\n %a")))
  ;; the todo-states of my gtd-system
  (setq org-todo-keywords 
	'((sequence "NEXT(n)" "WAITING(w!/!)" "SCHEDULED(a)" "SOMEDAY(s!/!)" "|" 
		    "DONE(d)" "CANCELLED(c)")))
  ;; add effort estimate standards
  (add-to-list 'org-global-properties
	       '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
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
	  ("gh" "Home" tags-todo "@home/+NEXT|+WAITING")
	  ("gu" "University" tags-todo "@uni/+NEXT|+WAITING")
	  ("gs" "Stad" tags-todo "@errands/+NEXT|+WAITING")
	  ("gl" "Laboratory" tags-todo "@lab/+NEXT|+WAITING")
	  ("gc" "Computer" tags-todo "@computer/+NEXT|+WAITING")
	  ("gi" "Internet" tags-todo "@internet/+NEXT|+WAITING")
	  ("ge" "Email" tags-todo "@email/+NEXT|+WAITING")
	  ("gb" "Bellen" tags-todo "@bellen/+NEXT|+WAITING")
	  ("ga" "Agenda" tags-todo "@agenda/+NEXT|+WAITING")
	  ("U" "Work: all UU-contexts"
	   (AREA="work"&(tags-todo "@uni/+NEXT|+WAITING")
		 (tags-todo "@lab/+NEXT|+WAITING")
		 (tags-todo "@computer/+NEXT|+WAITING")
		 (tags-todo "@internet/+NEXT|+WAITING")
		 (tags-todo "@email/+NEXT|+WAITING")
		 (tags-todo "@bellen/+NEXT|+WAITING")
		 (tags-todo "@agenda/+NEXT|+WAITING"))
	   nil)
	  ("H" "Home: all personal contexts" 
	   (AREA="personal"&(tags-todo "@home/+NEXT|+WAITING")
		 (tags-todo "@errands/+NEXT|+WAITING")
		 (tags-todo "@computer/+NEXT|+WAITING")
		 (tags-todo "@internet/+NEXT|+WAITING")
		 (tags-todo "@email/+NEXT|+WAITING")
		 (tags-todo "@bellen/+NEXT|+WAITING")
		 (tags-todo "@agenda/+NEXT|+WAITING"))
	   nil)
	  ("n" todo "NEXT" nil)
	  ("w" todo "WAITING" nil)
	  ("s" todo "SOMEDAY" nil)
	  ("d" "Agenda + Next actions" ((agenda) (todo "NEXT")))))
  (setq org-tag-alist '(("@home" . ?h)("@uni" . ?u)("@errands" . ?s)("@lab" . ?l)
			("@computer" . ?c)("@internet" . ?i)("@email" . ?e)
			("@bellen" . ?b)("@agenda" . ?a)))
  ;; extra org settings
  (setq org-return-follows-link t)
  (setq org-hide-leading-stars t)
  (setf org-special-ctrl-a/e t)
  (setq org-fontify-emphasized-text t) 
  (setq org-fontify-done-headline t)
  (setq org-directory "~/Dropbox/Apps/orgzly/")
					;(setq org-adapt-indentation t) ; makes todo contents indent at headline level
  (setq org-agenda-prefix-format "  %-17:c%?-12t% s") 
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-agenda-include-all-todo nil)
  (setq org-log-done 'time)
  (setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
  (setq org-deadline-warning-days 14)
  (setf org-tags-column -65)
  (setf org-highlight-latex-and-related '(latex script entities))
  (setq org-latex-pdf-process
	'("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  (setq org-fontifywhole-heading-line t)
  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (define-key org-agenda-mode-map "j" 'evil-next-line)
	      (define-key org-agenda-mode-map "k" 'evil-previous-line))))

(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list
	'("◉" "◎" "⚫" "○" "►" "◇")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (tangotango)))
 '(custom-safe-themes
   (quote
    ("5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" default)))
 '(org-agenda-files
   (quote
    ("~/Dropbox/Apps/orgzly/inbox.org" "~/Dropbox/Apps/orgzly/todo.org" "~/Dropbox/Apps/orgzly/calendars/IljaKocken.org" "~/Dropbox/Apps/orgzly/calendars/marinesciences.org" "~/Dropbox/Apps/orgzly/calendars/assistant.org" "~/Dropbox/Apps/orgzly/calendars/ubv.org" "~/Dropbox/Apps/orgzly/calendars/options.org")))
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
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 113 :width normal)))))
