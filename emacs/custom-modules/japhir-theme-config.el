;;; japhir-theme-config.el --- theme settings        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilja Kocken

;; Author: Ilja Kocken
;; Keywords: theme

;;; Comments:

;;; Code:
(require 'modus-themes)
(require 'circadian)

(customize-set-variable 'modus-themes-bold-constructs t)
(customize-set-variable 'modus-themes-italic-constructs t)
(customize-set-variable 'modus-themes-mixed-fonts t)
(customize-set-variable 'modus-themes-subtle-line-numbers t)
(customize-set-variable 'modus-themes-tabs-accented t)
(customize-set-variable 'modus-themes-variable-pitch-ui t)
(customize-set-variable 'modus-themes-inhibit-reload t)
(customize-set-variable 'modus-themes-fringes '(subtle))
(customize-set-variable 'modus-themes-lang-checkers nil)
(customize-set-variable 'modus-themes-mode-line '(4))
(customize-set-variable 'modus-themes-markup '(background italic))
(customize-set-variable 'modus-themes-syntax '(faint))
(customize-set-variable 'modus-themes-intense-hl-line '(accented))
(customize-set-variable 'modus-themes-paren-match '(bold intense))
(customize-set-variable 'modus-themes-links '(neutral-underline background))
(customize-set-variable 'modus-themes-prompts '(intense bold))
(customize-set-variable 'modus-themes-completions '(opinionated))
(customize-set-variable 'modus-themes-mail-citations nil)
(customize-set-variable 'modus-themes-region '(bg-only no-extend))
(customize-set-variable 'modus-themes-diffs '(desaturated))
(customize-set-variable 'modus-themes-org-blocks 'tinted-background)
(customize-set-variable 'modus-themes-org-agenda
                        '((header-block . (variable-pitch 1.3))
                          (header-date . (grayscale workaholic bold-today 1.1))
                          (event . (accented varied))
                          (scheduled . rainbow)
                          (habit . traffic-light)))
(customize-set-variable 'modus-themes-headings
                        '((1 . (variable-pitch 1.3))
                          (2 . (variable-pitch 1.1))
                          (3 . (variable-pitch semibold))))

;; current location on earth for auto toggle between light and dark theme
;   (calendar-latitude 52.08)
;   (calendar-longitude 5.11)
(customize-set-variable 'calendar-latitude 21.25) ;; Honolulu
(customize-set-variable 'calendar-longitude -157.8)
   ;; (calendar-latitude 40.5) ;; New York
   ;; (calendar-longitude -74.5)
(customize-set-variable 'circadian-themes '((:sunrise . modus-operandi)
                                            (:sunset  . modus-vivendi)))
(circadian-setup)

;;; font settings
;; (set-face-attribute 'default nil :family "Noto Mono" :height 130)
(set-face-attribute 'default nil :family "Noto Sans Mono" :height 150)
(set-face-attribute 'variable-pitch nil :family "Noto Serif" :height 1.0)
;;Tex Gyre Pagella
(set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono" :height 1.0)

;;; set rainbow mode
(require 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'r-mode-hook 'rainbow-mode)

;; emojify
(require 'emojify)
(global-emojify-mode)

;; visual-fill-column
(require 'visual-fill-column)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(customize-set-variable 'global-visual-line-mode t)
(customize-set-variable 'split-window-preferred-function
                        'visual-fill-column-split-window-sensibly)
(customize-set-variable 'visual-fill-column-center-text t)

;; org-mode stuff
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-agenda-mode (lambda ()
                             (visual-line-mode -1)
                             (toggle-truncate-lines 1)))

(customize-set-variable 'org-tags-column -65)
(customize-set-variable 'org-startup-indented t)
(customize-set-variable 'org-startup-folded 'content)
(customize-set-variable 'org-agenda-block-separator "")
(customize-set-variable 'org-fontify-emphasized-text t)
      ;; org-fontify-todo-headline t ;; might be nice, but needs customisation of org-headline-todo face
(customize-set-variable 'org-fontify-whole-heading-line t)
(customize-set-variable 'org-fontify-quote-and-verse-blocks t)
(customize-set-variable 'org-pretty-entities t)
(customize-set-variable 'org-ellipsis "…") ;▼ … ◦
(customize-set-variable 'org-hide-emphasis-markers t)

(customize-set-variable 'org-src-fontify-natively t)
(customize-set-variable 'org-src-tab-acts-natively t)
(customize-set-variable 'org-src-window-setup 'current-window)

;;; org-todo faces
; org-todo-next
  (defface org-todo-next
    '((((class color) (min-colors 16) (background light))
       (:family "Noto Sans Mono" :foreground "light goldenrod yellow"
		:bold t :background "red"))
      (((class color) (min-colors 16) (background dark))
       (:family "Noto Sans Mono" :foreground "light goldenrod yellow"
		:bold t :background "red"))
      (((class color) (min-colors 8) (background light))
       (:family "Noto Sans Mono" :foreground "light goldenrod yellow"
		:bold t :background "red"))
      (((class color) (min-colors 8) (background dark))
       (:family "Noto Sans Mono" :foreground "light goldenrod yellow"
		:bold t :background "red"))
      (t (:inverse-video t :bold t)))
      "Face for NEXT TODO keyword"
      :group 'org-faces)

; org-todo-waiting
  (defface org-todo-waiting
    '((((class color) (min-colors 16) (background light))
       (:family "Noto Sans Mono" :foreground "dim gray" :bold t
       :background "yellow"))
      (((class color) (min-colors 16) (background dark))
       (:family "Noto Sans Mono" :foreground "dim gray" :bold t
       :background "yellow"))
      (((class color) (min-colors 8) (background light))
       (:family "Noto Sans Mono" :foreground "dim gray" :bold t
       :background "yellow"))
      (((class color) (min-colors 8) (background dark))
       (:family "Noto Sans Mono" :foreground "dim gray" :bold t
       :background "yellow"))
      (t (:inverse-video t :bold t)))
    "Face for WAIT TODO keyword"
    :group 'org-faces)

; org-todo-tick
  (defface org-todo-tick
    '((((class color) (min-colors 16) (background light))
       (:family "Noto Sans Mono" :bold t :background "light slate blue"))
      (((class color) (min-colors 16) (background dark))
       (:family "Noto Sans Mono" :bold t :background "light slate blue"))
      (((class color) (min-colors 8)  (background light))
       (:family "Noto Sans Mono" :bold t :background "light slate blue"))
      (((class color) (min-colors 8)  (background dark))
       (:family "Noto Sans Mono" :bold t :background "light slate blue"))
      (t (:inverse-video t :bold t)))
    "Face for TICK TODO keyword"
    :group 'org-faces)

; org-todo-someday
  (defface org-todo-someday
    '((((class color) (min-colors 16) (background light))
       (:family "Noto Sans Mono" :foreground "ghost white" :bold t
       :background "deep sky blue"))
      (((class color) (min-colors 16) (background dark))
       (:family "Noto Sans Mono" :foreground "ghost white" :bold t
       :background "deep sky blue"))
      (((class color) (min-colors 8) (background light))
       (:family "Noto Sans Mono" :foreground "ghost white" :bold t
       :background "deep sky blue"))
      (((class color) (min-colors 8) (background dark))
       (:family "Noto Sans Mono" :foreground "ghost white" :bold t
       :background "deep sky blue"))
      (t (:inverse-video t :bold t)))
    "Face for SOME TODO keyword"
    :group 'org-faces)

; org-done-done
  (defface org-done-done
    '((((class color) (min-colors 16) (background light))
       (:family "Noto Sans Mono" :foreground "green4" :bold t
       :background "pale green"))
      (((class color) (min-colors 16) (background dark))
       (:family "Noto Sans Mono" :foreground "green4" :bold t
       :background "pale green"))
      (((class color) (min-colors 8))
       (:family "Noto Sans Mono" :foreground "green"
       :background "pale green"))
      (t (:bold t)))
    "Face used for todo keywords that indicate DONE items."
    :group 'org-faces)

; org-done-cancelled
  (defface org-done-cancelled
    '((((class color) (min-colors 16) (background light))
       (:family "Noto Sans Mono" :foreground "dim gray" :bold t
       :background "gray"))
      (((class color) (min-colors 16) (background dark))
       (:family "Noto Sans Mono" :foreground "dim gray" :bold t
       :background "gray"))
      (((class color) (min-colors 8))
       (:family "Noto Sans Mono" :foreground "dim gray"
       :background "gray"))
      (t (:bold t)))
    "Face used for todo keywords that indicate CANC items."
    :group 'org-faces)

(require 'org-appear) ; dive into links/formatted entries
(add-hook 'org-mode-hook 'org-appear-mode)
(customize-set-variable 'org-appear-autoentities t)
(customize-set-variable 'org-appear-autolinks t)
(customize-set-variable 'org-appear-autosubmarkers t)
(customize-set-variable 'org-appear-delay 1)

;; (require 'org-fragtog) ; auto-toggle latex fragments
;; (add-hook 'org-mode-hook 'org-fragtoc-mode)
;; (customize-set-variable 'org-format-latex-options
;;                         (plist-put org-format-latex-options :scale 2.5))

(require 'org-modern)
(customize-set-variable 'global-org-modern-mode 1)
(setq org-modern-todo-faces
      '(("NEXT" . org-todo-next)
        ("WAIT" . org-todo-waiting)
        ("TICK" . org-todo-tick)
        ("SOME" . org-todo-someday)
        ("DONE" . org-done-done)
        ("CANC" . org-done-cancelled)))

(provide 'japhir-theme-config)
;;; japhir-theme-config.el ends here
