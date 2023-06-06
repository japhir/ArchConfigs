;;; japhir-gtd-packages.el --- Completion packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: Ilja J. Kocken

;;; Commentary:

;; Add gtd packages to the list of packages to install.

;;; Code:

(add-to-list 'package-selected-packages 'org)
(add-to-list 'package-selected-packages 'ox)
(add-to-list 'package-selected-packages 'ox-latex)
(add-to-list 'package-selected-packages 'org-contrib)
(add-to-list 'package-selected-packages 'ob-org)
(add-to-list 'package-selected-packages 'ob-async)
(add-to-list 'package-selected-packages 'org-pomodoro)
(add-to-list 'package-selected-packages 'org-protocol)
(add-to-list 'package-selected-packages 'org-roam)

(provide 'japhir-gtd-packages)
;;; japhir-gtd-packages.el ends here
