;; package -- ChlorophyII's init.el
;;; Commentary:
;;    Warning: When running the first time on Linux, use "emacs -nw"
;;    GUI will fail to install some packages
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
		 '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; =============================================================================

;; To autoload bash init file
(shell-command "source ~/.bashrc")

;; To enable or disable web development packages
(defvar web-development nil)

;; ChezScheme is ChlorophyII's default option
(defvar scheme-program-name "chez")

(defvar python-shell-interpreter "python3")

;; =============================================================================

;; Notice the slash after "texbin"
(setenv "PATH" (concat
		"/usr/local/bin:"
		"/usr/local/sbin:"
		"/Library/TeX/texbin/:"
		(getenv "PATH")))
(setq exec-path (split-string (getenv "PATH") path-separator))

;; =============================================================================

;; To avoid a stupid bug of package-install caused by empty archive
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; =============================================================================

(unless (package-installed-p 'zenburn-theme)
  (package-install 'zenburn-theme))
(load-theme 'zenburn t)

(set-cursor-color "#7F9F7F") ; zenburn-green
(setq-default cursor-type 'box) ; bar

(cond ((eq system-type 'darwin)
       (set-face-attribute 'default nil :font "menlo-14")
       (setq-default line-spacing 2))
      ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
       (setq-default line-spacing 1)))
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 140))

;; Don't let Emacs hurt your ears
(setq visible-bell t)

(if (display-graphic-p) ; GUI
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

(menu-bar-mode 0)

;; Merge the fringe with background
(set-face-attribute 'fringe nil
		    :foreground (face-foreground 'default)
		    :background (face-background 'default))

(setf (cdr (assq 'continuation fringe-indicator-alist))
				   ;; '(nil nil) ;; no continuation indicators
				   '(nil right-curly-arrow) ;; right indicator only
				   ;; '(left-curly-arrow nil) ;; left indicator only
				   ;; '(left-curly-arrow right-curly-arrow) ;; default
				   )

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message ()
  "Appear in the echo area when starting Emacs."
  (message "Emacs Rocks!"))

;; Wrap lines in org-mode
(defvar org-startup-truncated nil)

;; =============================================================================

;; Warning: Don't touch the code below
;; AUCTeX will not function even a single character is missed

(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(require 'tex)

(defvar LaTeX-electric-left-right-brace t)
(defvar TeX-electric-math t)
;; To use AUCTeX preview, gs is needed
(cond ((eq system-type 'darwin)
       (setq preview-gs-command "/usr/local/bin/gs"))
      ((eq system-type 'gnu/linux)
       (setq preview-gs-command "/usr/bin/gs")))

(defvar preview-auto-cache-preamble t)

(eval-after-load "latex"
  '(add-to-list 'LaTeX-verbatim-macros-with-braces "hphantom"))
(eval-after-load "latex"
  '(add-to-list 'LaTeX-verbatim-macros-with-braces "vphantom"))

(setq TeX-PDF-mode t)

(use-package latex-extra ; Adds several useful functionalities to LaTeX-mode.
  :ensure t
  :hook ((LaTeX-mode . latex-extra-mode)
	 (LaTeX-mode . turn-off-auto-fill))
  :init (add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)) t)
  ;; DO NOT TOUCH THE PREVIOUS LINE (TURN OFF F**KING ANNOYING AUTO-FILL-MODE)
  :config (setq latex/override-preview-map nil))

;; =============================================================================

;; Macaulay 2 start
(load "~/.emacs-Macaulay2" t)
;; Macaulay 2 end

;; =============================================================================

(unless (package-installed-p 'matlab-mode)
  (package-install 'matlab-mode))

(when web-development
  (use-package web-mode
    ;; Major mode for editing web templates
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

  (use-package sql-indent
    ;; Indentation of SQL statements
    :ensure t
    :config
    (with-eval-after-load "sql"
      (load-library "sql-indent"))
    (defvar sql-indent-offset 2))

  (use-package impatient-mode
    ;; Serve buffers live over HTTP
    :ensure t
    :hook ((web-mode css-mode) . impatient-mode))

  (use-package rainbow-mode
    ;; Colorize color names in buffers
    :ensure t
    :hook css-mode))

(use-package flycheck
  ;; On-the-fly syntax checking
  :ensure t
  :config
  (global-flycheck-mode t))

(use-package company
  ;; Modular text completion framework
  :ensure t
  :config
  (global-company-mode t)
  (setq company-backends
	'((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf
           company-yasnippet)
          (company-abbrev company-dabbrev))))

(use-package company-auctex
  ;; Company-mode auto-completion for AUCTeX
  :ensure t
  :hook ((LaTeX-mode . company-auctex-init)
	 (LaTeX-mode . (lambda ()
			 (add-to-list (make-local-variable 'company-backends)
				      'company-auctex)))))

(use-package company-math
  ;; Completion backends for unicode math symbols and latex tags
  :ensure t
  :hook (LaTeX-mode . (lambda ()
			(if (display-graphic-p)
			    (add-to-list (make-local-variable 'company-backends)
					 '(company-math-symbols-latex
					   company-latex-commands))))))

(use-package multiple-cursors
  ;; Multiple cursors for Emacs
  :ensure t
  :hook ((prog-mode web-mode) . multiple-cursors-mode)
  :config
  (global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-S-p") 'mc/mark-previous-like-thismc)
  (global-set-key (kbd "C-c C-S-n") 'mc/mark-all-like-this))

(use-package move-text
  ;; Move current line or region with M-up or M-down.
  :ensure t
  :config
  (move-text-default-bindings))

(use-package all-the-icons
  ;; A library for inserting Developer icons
  :ensure t
  :config
  (unless (package-installed-p 'all-the-icons)
    (all-the-icons-install-fonts "yes")))

(use-package neotree
  ;; A tree plugin like NerdTree for Vim
  :ensure t
  :config
  (setq neo-theme 'icons))

(use-package fill-column-indicator
  ;; Graphically indicate the fill column
  :ensure t
  :hook (prog-mode . fci-mode)
  :config
  (setq fci-rule-color "#4f4f4f")
  (setq fci-rule-column 80))

(use-package magit
  ;; A Git porcelain inside Emacs.
  :ensure t)

(use-package markdown-mode
  ;; Major mode for Markdown-formatted text
  :ensure t)

(use-package csv-mode
  ;; Major mode for editing comma/char separated values
  :ensure t)

(use-package pyvenv
  ;; Python virtual environment interface
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" "~/"))

(use-package wc-mode
  ;; Running word count with goals (minor mode)
  :ensure t
  :hook text-mode
  :config
  (setq wc-modeline-format "WC[%w %tw %tc]"))

(use-package writeroom-mode
  ;; Minor mode for distraction-free writing
  :ensure t
  :hook ((writeroom-mode . (lambda ()
			     (writeroom-adjust-width
			      (truncate (- (* 0.52 (/ (display-pixel-width)
						     (frame-char-width)))
					   visual-fill-column-width)))))
	 (writeroom-mode . (lambda () (fci-mode 0)))
	 (writeroom-mode . (lambda ()
			     (setf (cdr (assq 'continuation
					      fringe-indicator-alist))
				   '(nil nil) ;; no continuation indicators
				   )))))

(use-package highlight-parentheses
  ;; Highlight surrounding parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode))

(use-package highlight-numbers
  ;; Highlight numbers in source code
  :ensure t
  :hook ((prog-mode matlab-mode) . highlight-numbers-mode))

(use-package fic-mode
  ;; Show FIXME/TODO/BUG(...) in special face only in comments and strings
  :ensure t
  :hook ((prog-mode LaTeX-mode) . fic-mode))

;; =============================================================================

(defun add-hook-list (mode-hook hook-list)
  "Add hooks to MODE-HOOK from HOOK-LIST."
  (dolist (hook hook-list)
    (add-hook mode-hook hook)))

(defun my-hs-minor-mode-hook ()
  "Change keybindings of hs-show and hs-hide."
  (hs-minor-mode t)
  (local-set-key (kbd "C-+") 'hs-show-all) ;; ctrl+shift+=
  (local-set-key (kbd "C-_") 'hs-hide-all)   ;; ctrl+shift+-
  (local-set-key (kbd "C-=") 'hs-show-block)
  (local-set-key (kbd "C--") 'hs-hide-block))

(add-hook-list 'prog-mode-hook
	       (list #'linum-mode
		     #'electric-pair-mode
		     #'my-hs-minor-mode-hook))

(add-hook-list 'matlab-mode-hook
	       (list #'linum-mode))

(add-hook-list 'LaTeX-mode-hook
	       (list #'linum-mode
		     (lambda () (set (make-variable-buffer-local
				      'TeX-electric-math)
				     (cons "$" "$")))))
(add-hook-list 'doc-view-mode-hook
	       (list #'auto-revert-mode))

(when web-development
  (add-hook-list 'web-mode-hook
		 (list #'httpd-start))
  (add-hook-list 'css-mode-hook
		 (list #'httpd-start)))

;; =============================================================================

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; =============================================================================

(defun icloud ()
  "Change working directory to iCloud drive."
  (interactive)
  (cd "~/Library/Mobile Documents/com~apple~CloudDocs"))

;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pyvenv all-the-icons ebib writeroom-mode fic-mode fill-column-indicator magit move-text multiple-cursors zenburn-theme wc-mode rainbow-mode markdown-mode impatient-mode neotree highlight-parentheses flycheck auctex use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
