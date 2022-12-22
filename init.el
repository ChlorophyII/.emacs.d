;; package -- ChlorophyII's init.el
;;; Commentary:
;;    Warning: When running the first time on Linux, use "emacs -nw"
;;    GUI will fail to install some packages
;;; Code:

;; =============================================================================

; To speed up start-up
(setq gc-cons-threshold 50000000)

;; =============================================================================

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
(if (version< emacs-version "27")
    (package-initialize))

;; =============================================================================

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; =============================================================================

; To solve path-related problems
(use-package exec-path-from-shell
  ;; Get environment variables such as $PATH from the shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
	    (exec-path-from-shell-initialize)))

;; =============================================================================

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(set-cursor-color "#7F9F7F") ; zenburn-green
(setq-default cursor-type 'box) ; bar

(cond ((eq system-type 'darwin)
       (set-face-attribute 'default nil :font "menlo-14")
       (setq-default line-spacing 2))
      ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
       (setq-default line-spacing 1)))
(with-eval-after-load "linum"
  (set-face-attribute 'linum nil :height 140))

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

;; Adaptively indent text in org-mode
(defvar org-adapt-indentation t)

;; =============================================================================

(use-package tex
  ;; Integrated environment for *TeX*
  :ensure auctex
  :defer t
  :config

  (set-default 'preview-scale-function 1.2)

  (defvar preview-auto-cache-preamble t)
  (with-eval-after-load "latex"
    (add-to-list 'LaTeX-verbatim-macros-with-braces "hphantom")
    (add-to-list 'LaTeX-verbatim-macros-with-braces "vphantom"))
  (setq TeX-PDF-mode t))

(use-package latex-extra ; Adds several useful functionalities to LaTeX-mode.
  :ensure t
  :defer t
  :hook ((LaTeX-mode . latex-extra-mode)
	 (LaTeX-mode . turn-off-auto-fill))
  :init (add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)) t)
  ;; DO NOT TOUCH THE PREVIOUS LINE (TURN OFF F**KING ANNOYING AUTO-FILL-MODE)
  :config (setq latex/override-preview-map nil))

;; =============================================================================

(use-package esup
  ;; Benchmark Emacs Startup time without ever leaving your Emacs.
  :ensure t
  :defer t
  :config (setq esup-depth 0))

(use-package flycheck
  ;; On-the-fly syntax checking
  :ensure t
  :hook
  (c++-mode . (lambda () (setq flycheck-clang-language-standard "c++11")))
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
  :defer t
  :hook ((LaTeX-mode . company-auctex-init)
	 (LaTeX-mode . (lambda ()
			 (add-to-list (make-local-variable 'company-backends)
				      'company-auctex)))))

(use-package company-math
  ;; Completion backends for unicode math symbols and latex tags
  :ensure t
  :defer t
  :hook (LaTeX-mode . (lambda ()
			(if (display-graphic-p)
			    (add-to-list (make-local-variable 'company-backends)
					 '(company-math-symbols-latex
					   company-latex-commands))))))

(use-package jedi
  ;; a Python auto-completion for Emacs
  :ensure t
  :hook (python-mode . jedi:setup)
  )

(use-package smartparens
  ;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
  :ensure t
  :hook ((prog-mode text-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\\[" "\\]"))

(use-package move-text
  ;; Move current line or region with M-up or M-down.
  :ensure t
  :config
  (move-text-default-bindings))

(use-package magit
  ;; A Git porcelain inside Emacs.
  :ensure t
  :defer t)

(use-package markdown-mode
  ;; Major mode for Markdown-formatted text
  :ensure t
  :defer t)

(use-package racket-mode
  ;; Racket editing, REPL, and more
  :ensure t
  :defer t)

(use-package csv-mode
  ;; Major mode for editing comma/char separated values
  :ensure t
  :defer t)

(use-package pyvenv
  ;; Python virtual environment interface
  :ensure t
  :defer t
  :hook (python-mode . pyvenv-mode)
  :config
  (setenv "WORKON_HOME" "~/"))

(use-package neotree
  ;; A tree plugin like NerdTree for Vim
  :ensure t
  :config
  (setq neo-theme 'icons))

(use-package all-the-icons
  ;; A library for inserting Developer icons
  :ensure t
  :config
  (unless (package-installed-p 'all-the-icons)
    (all-the-icons-install-fonts "yes")))

(use-package fill-column-indicator
  ;; Graphically indicate the fill column
  :ensure t
  :hook (prog-mode . fci-mode)
  :config
  (setq fci-rule-color "#4f4f4f")
  (setq fci-rule-column 80))

(use-package wc-mode
  ;; Running word count with goals (minor mode)
  :ensure t
  :defer t
  :hook text-mode
  :config
  (setq wc-modeline-format "WC[%w %tw %tc]"))

(use-package writeroom-mode
  ;; Minor mode for distraction-free writing
  :ensure t
  :defer t
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
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))

(use-package highlight-numbers
  ;; Highlight numbers in source code
  :ensure t
  :defer t
  :hook (prog-mode . highlight-numbers-mode))

(use-package fic-mode
  ;; Show FIXME/TODO/BUG(...) in special face only in comments and strings
  :ensure t
  :defer t
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

(defun my-LaTeX-keybindings-hook ()
  "Add keybindings for autobrackets and insert braces after ^ and _."
  (local-set-key (kbd "C-c a") (lambda () (interactive)
				 (progn (insert "\\abra{}")
					(backward-char))))
  (local-set-key (kbd "C-c c") (lambda () (interactive)
				 (progn (insert "\\abrc{}")
					(backward-char))))
  (local-set-key (kbd "C-c r") (lambda () (interactive)
				 (progn (insert "\\abrr{}")
					(backward-char))))
  (local-set-key (kbd "C-c s") (lambda () (interactive)
				 (progn (insert "\\abrs{}")
					(backward-char))))
  (local-set-key (kbd "^") (lambda () (interactive)
			     (progn (insert "^{}")
				    (backward-char))))
  (local-set-key (kbd "_") (lambda () (interactive)
			     (progn (insert "_{}")
				    (backward-char)))))

(defun my-org-smartparens-setting-hook ()
  "Remove unnecessary pairs."
  (sp-local-pair 'org-mode "*" nil :actions :rem)
  (sp-local-pair 'org-mode "_" nil :actions :rem)
  (sp-local-pair 'org-mode "=" nil :actions :rem)
  (sp-local-pair 'org-mode "~" nil :actions :rem)
  (sp-local-pair 'org-mode "/" nil :actions :rem))

(defun my-org-keybindings-hook ()
  "Add keybindings to `org-mode'."
  (local-set-key (kbd "C-c e")
		 (lambda () (interactive)
		   (progn (indent-for-tab-command)
			  (insert "#+BEGIN_EXPORT latex")
			  (newline-and-indent 2)
			  (insert "#+END_EXPORT")
			  (forward-line -1)
			  (indent-for-tab-command)
			  (org-edit-export-block)))))

(defun my-org-deeper-subsection-class-hook ()
  "Add an extra level of subsection when exporting org to LaTeX."
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
               '("article-deeper-subsections"
		 "\\documentclass{article}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(defun my-org-report-paragraph-class-hook ()
  "Use paragraph in report when exporting org to LaTeX."
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
               '("report-paragraph"
		 "\\documentclass{report}"
		 ("\\part{%s}" . "\\part*{%s}")
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(add-hook-list 'prog-mode-hook
	       (list #'linum-mode
		     #'my-hs-minor-mode-hook))

(add-hook-list 'LaTeX-mode-hook
	       (list #'linum-mode
		     #'my-LaTeX-keybindings-hook))
(add-hook-list 'org-mode-hook
	       (list #'my-LaTeX-keybindings-hook
		     #'my-org-smartparens-setting-hook
		     #'my-org-keybindings-hook
		     #'my-org-deeper-subsection-class-hook
		     #'my-org-report-paragraph-class-hook))

(add-hook-list 'doc-view-mode-hook
	       (list #'auto-revert-mode))

;; =============================================================================

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; =============================================================================

;; ChezScheme is ChlorophyII's default option
(defvar scheme-program-name "scheme")

(defvar python-shell-interpreter "python3")

;; Macaulay 2
(load "~/.emacs-Macaulay2" t)

(defun icloud ()
  "Change working directory to iCloud drive."
  (interactive)
  (cd "~/Library/Mobile Documents/com~apple~CloudDocs"))

;; =============================================================================

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)))

;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(graphql graphql-mode ess graphviz-dot-mode pyvenv all-the-icons ebib writeroom-mode fic-mode fill-column-indicator magit move-text multiple-cursors zenburn-theme wc-mode rainbow-mode markdown-mode impatient-mode neotree highlight-parentheses flycheck auctex use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
