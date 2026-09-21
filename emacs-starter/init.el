;;; init.el --- bootstrap: tangle and load config.org  -*- lexical-binding: t -*-

;; Everything lives in config.org next to this file.  On every start
;; Emacs tangles it to config.el and loads that.  Edit config.org, not
;; config.el.

(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
