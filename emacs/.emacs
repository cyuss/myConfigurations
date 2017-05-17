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
                          dashboard
                          deft
                          erlang
                          expand-region
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
                          iedit
                          latex-extra
                          latex-preview-pane
                          magit
                          markdown-mode
                          markdown-mode
                          markdown-mode+
                          markdown-toc
                          marmalade
                          multiple-cursors
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
                          use-package
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
;;(setq py-indent-offset 4)
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

;; disable tool bar mode
(tool-bar-mode -1)

;; disable menu bar mode
(menu-bar-mode -99)

(require 'dashboard)
(dashboard-setup-startup-hook)

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'ace-mc)
(global-set-key (kbd "C-)") 'ace-mc-add-multiple-cursors)
(global-set-key (kbd "C-M-)") 'ace-mc-add-single-cursor)

;; iedit package
(require 'iedit)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; configure ace-jum-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; helm customization
(require 'helm)
(require 'helm-config)

;;(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key help-map (kbd "C-z") 'helm-select-action) ;; list actions
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-i") 'helm-swoop)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;;(helm-autoresize-mode 1) ;; active autoresize mode
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ;; list kill ring cases
(global-set-key (kbd "C-c h o") 'helm-occur) ;; find pattern occurences

;; ace-window package
;;(require 'ace-window)
;;(global-set-key (kbd "M-p") 'ace-window)

;; key chords
(require 'key-chord)
;;(setq key-chord-two-keys-delay 0.1) ; default 0.1
;;(setq key-chord-one-key-delay 0.2) ; default 0.2

;;(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "df" 'forward-char)
(key-chord-define-global "jk" 'backward-char)

(key-chord-mode +1)

;; keybindings for company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-,") 'company-complete-common)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; add some shortcuts to clean buffer and region
(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
;;(global-set-key (kbd "M-n") 'next-line)
;;(global-set-key (kbd "M-p") 'previous-line)

;; scroll bar mode (disable)
(scroll-bar-mode -1)

;; rainbow identifiers mode - highlighting based on names
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

(setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)

;; global org-mode configuration
(require 'ox-latex)
(require 'ox-html)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; add linum-highlight-current-line-number.el file to .emacs.d directory
(require 'linum)

(defvar linum-current-line 1 "Current line number.")
(defvar linum-border-width 1 "Border width for linum.")

(defface linum-current-line
  `((t :inherit linum
       :foreground "goldenrod"
       :weight bold
       ))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)
        ;; It's the same algorithm that linum dynamic. I only had added one
        ;; space in front of the first digit.
        linum-border-width (number-to-string
                            (+ 1 (length
                                  (number-to-string
                                   (count-lines (point-min) (point-max))))))))

(defun linum-highlight-current-line (line-number)
  "Highlight the current line number using `linum-current-line' face."
  (let ((face (if (= line-number linum-current-line)
                  'linum-current-line
                'linum)))
    (propertize (format (concat "%" linum-border-width "d") line-number)
                'face face)))

(setq linum-format 'linum-highlight-current-line)