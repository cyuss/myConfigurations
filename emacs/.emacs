;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(load "package")
(require 'cl)

(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

;; use-package installation
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; enable use-package
(eval-when-compile
  (require 'use-package))
;;(require 'diminish) ;; if we want to use :diminish
(require 'bind-key) ;; if we want to use :bind

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(set-default-font "Consolas-15")

;; theme installation
(use-package solarized-theme
  :ensure t
  :defer 10
  :init
  (setq solarized-use-variable-pitch nil)
)

;; start buffers
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
(progn
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

;; indentation
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)))

;; every time when neotree window is opened, let it find current file and jump to node
(setq neo-smart-open t)

(electric-indent-mode 1)

;; backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)
(setq delete-old-versions -1)

;; yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; key bindings
(global-set-key (kbd "C-!") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; misc
(show-paren-mode t)

;; column number mode
(setq column-number-mode t)

;; autopair mode
(require 'autopair)
(autopair-global-mode 1)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; highlight current line
(global-hl-line-mode +1)

;; windows configuration
(tool-bar-mode -1)
(display-time-mode 1)
(menu-bar-mode -99)
(scroll-bar-mode -1)

;; highlight surrounding parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
	(highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; miniedit
(use-package miniedit
  :commands minibuffer-edit
  :ensure t
  :init (miniedit-install))

(use-package anzu
  :init (global-anzu-mode 1)
  :diminish anzu-mode
  :ensure t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; multiple cursors
(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<" . mc/mark-all-like-this))
  )

;; ace multiple cursors
(use-package ace-mc
  :defer t
  :bind (("C-)" . ace-mc-add-multiple-cursors)
		 ("C-M-)" . ace-mc-add-single-cursor))
  )

;; iedit
(use-package iedit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; expand region
(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;; ace jump mode
(use-package ace-jump-mode
  :defer t
  :bind (("C-c SPC" . ace-jump-mode))
  )

;; ace window
(use-package ace-window
  :ensure
  ;;:defer t
  :bind (("M-p" . ace-window))
  )

;; key chord
(require 'key-chord)
;;(setq key-chord-two-keys-delay 0.1) ; default 0.1
;;(setq key-chord-one-key-delay 0.2) ; default 0.2
(key-chord-mode +1)
(key-chord-define-global "df" 'forward-char)
(key-chord-define-global "jk" 'backward-char)

;; company
(use-package company
  :config (add-hook 'prog-mode-hook 'company-mode)
  :bind (("C-," . company-complete-common)
		 :map company-active-map
		 ("C-n" . company-select-next)
		 ("C-p" . company-select-previous))
  )

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; magit
(use-package magit
  :defer t
  ;;:ensure t
  :bind ("C-x g" . magit-status))

;; rainbow identifiers mode
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
;; configuration
(setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)

;; org mode
(use-package org
  :defer t
  :mode ("\\.org" . org-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb))
  :config
  (setq org-src-window-setup 'current-window)
  (require 'org-ac)
  (defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 80))  ; or whatever width makes sense for you
    ad-do-it))
  (org-ac/config-default)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-log-mode-items '(closed clock state)) ;; to see done tasks in org-agenda (by pressing l)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (latex . t)
     (sh . t)
     (C . t)
     (awk . t)
     ))
  )

;; helm mode
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0
		  helm-input-idle-delay 0.01
		  helm-quick-update t
		  helm-M-x-requires-pattern nil
		  helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("M-x" . helm-M-x)
		 ("C-x b" . helm-mini)
		 ("C-x C-f" . helm-find-files)
		 ("M-i" . helm-swoop)
		 ("M-y" . helm-show-kill-ring)
		 ("C-c h o" . helm-occur)
		 ("M-D" . helm-buffer-run-kill-buffers)
		 :map helm-map
		 ("<tab>" . helm-execute-persistent-action)
		 ("C-<tab>" . helm-select-action)
		 :map isearch-mode-map
		 ("M-i" . helm-swoop-from-isearch))
  )

;; useful functions
;; insert date
(defun cyuss--insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; search all buffers
(defun cyuss--search-all-buffers (regexp) 
  "Search all open buffers for a regex. Open an occur-like window."
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))

;; make a temporary file
(defun cyuss--make-temp-file (name)
  "Creates a temporary file in the system temp directory, for various purposes."
  (interactive "sFile name:")
  (generate-new-buffer name)
  (switch-to-buffer name)
  (write-file (concat temporary-file-directory name)))

;; rename file and buffer
(defun cyuss--rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(unless filename
	  (error "Buffer '%s' is not visiting a file!" name))
	(if (get-buffer new-name)
		(message "A buffer named '%s' already exists!" new-name)
	  (progn
		(rename-file name new-name 1)
		(rename-buffer new-name)
		(set-visited-file-name new-name)
		(set-buffer-modified-p nil)))))

;; define the dependicies when working with python project
(defun cyuss--python-workenv ()
  "define my workflow in python"
  (interactive)
  (linum-mode 1)
  (projectile-mode 1)
  (elpy-enable)
  (elpy-mode 1)
  (yas/minor-mode t)
  (require 'sphinx-doc)
  (sphinx-doc-mode t))

;; undo-tree - visualize your undos and branches
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; swiper configuration
(use-package swiper
  :ensure t
  :bind ("C-c i" . swiper))
