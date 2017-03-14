;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; add elpy repo
(add-to-list 'package-archives
	        '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)
;; enable elpy package
;; packages to install: rope, jedi, flake8, importmagic, autopep8 and yapf
(elpy-enable)

(require 'cl)
;; change custom file configuration
(setq custom-file "~/.emacs-custom.el")
(load "~/.emacs-custom.el")


;; default font
(set-default-font "Consolas 16")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode writegood-mode web-mode vala-mode solarized-theme sml-mode smex scala-mode rvm restclient puppet-mode php-mode paredit o-blog nodejs-repl marmalade markdown-mode magit idris-mode htmlize highlight-parentheses haskell-mode haml-mode graphviz-dot-mode go-eldoc go-autocomplete gist flycheck feature-mode expand-region erlang elpy deft csharp-mode coffee-mode clojure-mode autopair auctex ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; list of default packages
(defvar youcef/packages '(ac-slime
                          auto-complete
                          autopair
                          clojure-mode
                          coffee-mode
                          csharp-mode
                          deft
                          erlang
                          feature-mode
                          flycheck
                          gist
                          go-autocomplete
                          go-eldoc
                          go-mode
                          graphviz-dot-mode
                          haml-mode
                          haskell-mode
                          htmlize
                          idris-mode
                          magit
                          markdown-mode
                          marmalade
                          nodejs-repl
                          o-blog
                          org
                          paredit
                          php-mode
                          puppet-mode
                          restclient
                          rvm
                          scala-mode
                          smex
                          sml-mode
                          solarized-theme
                          web-mode
                          writegood-mode
                          yaml-mode)
  "Default packages")

;; auto install default packages
(defun packages-installed-p ()
  (loop for pkg in youcef/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg youcef/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; skip straight to scratch buffer
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; end line marker
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; indentation
;;(setq tab-width 4 indent-tabs-mode t)

(setq-default tab-width 4)
;;(setq-default tab-always-indent 'complete)
(electric-indent-mode 1)
;;(setq python-indent-offset 4)

;; backup files
(setq make-backup-files nil)

;; yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; misc
(show-paren-mode t)

;; Ido: navigate filesystem
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; column number mode
(setq column-number-mode t)

;; autopair mode
(require 'autopair)
(autopair-global-mode 1)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; indentation and cleanup buffer
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; highlight surrounding parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
	highlight-parentheses-mode
	(lambda ()
		(highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; highlight current line
(global-hl-line-mode +1)