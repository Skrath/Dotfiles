(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrap my packages
;; See: https://github.com/jwiegley/use-package
(use-package afternoon-theme :ensure t)
(use-package auto-complete :ensure t)
(use-package cedet :ensure t)
(use-package crontab-mode :ensure t :defer t)
(use-package csv-mode :ensure t :defer t)
(use-package docker-tramp :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package ecb :ensure t)
(use-package edbi :ensure t)
(use-package epc :ensure t)
(use-package flymake-css :ensure t :defer t)
(use-package flymake-easy :ensure t :defer t)
(use-package flymake-php :ensure t :defer t)
(use-package geben :ensure t :defer t)
(use-package git-commit :ensure t :defer t)
(use-package gitconfig-mode :ensure t :defer t)
(use-package haml-mode :ensure t :defer t)
(use-package idomenu :ensure t)
(use-package markdown-mode :ensure t :defer t)
(use-package nginx-mode :ensure t :defer t)
(use-package org :ensure t :defer t)
(use-package php-mode :ensure t :defer t)
(use-package python-mode :ensure t :defer t)
(use-package regex-tool :ensure t :defer t)
(use-package sass-mode :ensure t :defer t)
(use-package ssh-config-mode :ensure t :defer t)
(use-package ssh-file-modes :ensure t :defer t)
(use-package vcl-mode :ensure t :defer t)
(use-package visual-regexp :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package yasnippet :ensure t :defer t)


(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)

(semantic-mode 1)
(global-ede-mode 1)

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
(add-hook 'c-mode-common-hook 'my-cedet-hook)


(setq load-path (delete "/usr/share/emacs/24.3/lisp/cedet" load-path))

(global-auto-revert-mode t)

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq stack-trace-on-error t)

(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;; Debug a simple PHP script.
;; Change the session key my-php-54 to any session key text you like
(defun my-php-debug ()
  "Run current PHP script for debugging with geben"
  (interactive)
  (call-interactively 'geben)
  (shell-command
    (concat "XDEBUG_CONFIG='idekey=my-php-54' /home/chrisrasys/php54/bin/php "
    (buffer-file-name) " &"))
  )

(global-set-key [f5] 'my-php-debug)

(require 'epc)
(require 'edbi)

(load "cl-macs" nil t) ; No provide in this file.

;; Ido
(require 'ido) (ido-mode t)
(defadvice ido-find-file (before auto-refresh-ido nil activate)
  (setq ido-rescan t))
(autoload 'idomenu "idomenu" nil t)

(global-set-key "\C-ca" 'org-agenda)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (afternoon)))
 '(desktop-save-mode t)
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(ecb-auto-activate t)
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes (quote (("left15" (ecb-directories-buffer-name 0.17703349282296652 . 0.4807692307692308) (ecb-methods-buffer-name 0.17703349282296652 . 0.5)) ("left7" (ecb-directories-buffer-name 0.19617224880382775 . 0.5) (ecb-history-buffer-name 0.19617224880382775 . 0.23076923076923078) (ecb-methods-buffer-name 0.19617224880382775 . 0.25)))))
 '(ecb-major-modes-show-or-hide (quote (nil git-commit-mode)))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-use-speedbar-instead-native-tree-buffer nil)
 '(ede-project-directories (quote ("/home/chris/projects" "/home/chrisrasys/projects/srl.dev")))
 '(geben-pause-at-entry-line t)
 '(global-auto-complete-mode t)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(global-semantic-show-unmatched-syntax-mode t)
 '(inhibit-startup-screen t)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5)))
 '(org-agenda-custom-commands (quote (("D" "Daily Task List" ((alltodo "" nil) (agenda "" ((org-agenda-sorting-strategy (quote (tag-up))) (org-agenda-span (quote day)) (org-deadline-warning-days 0)))) nil nil))))
 '(org-agenda-files (quote ("~/Documents/org" "~/Documents/org/clients")))
 '(org-agenda-include-all-todo t)
 '(org-agenda-start-with-clockreport-mode t)
 '(org-agenda-timegrid-use-ampm t)
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clocktable-defaults (quote (:maxlevel 4 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))
 '(org-column ((t (:background "grey10" :strike-through nil :underline nil :slant normal :weight normal :height 98 :family "DejaVu Sans Mono"))))
 '(org-columns-default-format "%45ITEM(Details) %DEADLINE(Due) %ALLTAGS(Context) %7TODO(Status) %7Effort(Time){:} %7CLOCKSUM{Total}")
 '(org-default-notes-file "~/Documents/org/tasks.org")
 '(org-directory "~/Documents/org")
 '(org-enforce-todo-dependencies t)
 '(org-global-properties (quote (("Effort_ALL" . "1:00 2:00 3:00 4:00 5:00"))))
 '(org-hide-leading-stars t)
 '(org-hierarchical-todo-statistics nil)
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(org-todo-keywords (quote ((sequence "TODO" "IN PROGRESS" "DONE"))))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(save-place t nil (saveplace))
 '(semantic-mode t)
 '(show-paren-mode t)
 '(sr-speedbar-right-side nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "green"))) t)
 '(diff-indicator-added ((t (:inherit diff-added))) t)
 '(diff-indicator-removed ((t (:inherit diff-removed))) t)
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))) t))


(if (eq window-system 'x)
    (set-face-attribute 'default nil :font "Inconsolata-13")
  ;; (color-theme-midnight)
)

;; Autocomplete -- this needs to occur after setting color-theme
;; options for some reason (require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     (expand-file-name
	      "~/.emacs.d/elpa/auto-complete-20130503.2013/dict"))
(setq ac-comphist-file (expand-file-name
			"~/.emacs.d/ac-comphist.dat"))
(add-to-list 'ac-sources 'ac-source-semantic)
(local-set-key (kbd "C-:") 'semantic-ia-complete-symbol-menu) ; set shortcut for auto completion.
(local-set-key (kbd "C-.") 'ac-complete-semantic)
(ac-config-default)


(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defun unfill-region (beg end)
      "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
      (interactive "*r")
      (let ((fill-column (point-max)))
        (fill-region beg end)))

;; configure css-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

(add-hook 'php-mode-user-hook 'turn-on-font-lock)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'yasnippet)
(yas-global-mode 1)

;; Magento stuff
(defvar n98-magerun-executable "/usr/local/bin/n98-magerun.phar")

(defun magerun-commands()
  (setq magerun-commands '())
  (with-temp-buffer
    (insert (shell-command-to-string n98-magerun-executable))
    (goto-char (point-min))
    (let ((cmd-start-bound (search-forward-regexp "^admin" (point-max) t)))
      (goto-char cmd-start-bound)
      (while (re-search-forward "^  [a-zA-Z:-]+" nil t)
        (add-to-list 'magerun-commands (s-trim (match-string 0))))))
  magerun-commands)


(defun n98-magerun-run-command()
  (interactive)
  (let ((cmd (ido-completing-read "n98-magerun: " (magerun-commands))))
    (async-shell-command (format "%s %s" n98-magerun-executable cmd))))

;; (require 're-builder)
;; (setq reb-re-syntax 'string)
