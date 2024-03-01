;;; package --- Summary
;; ChlorophyII's init.el
;;; Commentary:
;; Emacs 29 required.
;;; Code:

;; =============================================================================

;; To speed up start-up
(setq gc-cons-threshold 50000000)

;; =============================================================================

;; To solve path-related problems
(use-package exec-path-from-shell
  ;; Get environment variables such as $PATH from the shell
  :ensure t
  :when (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package emacs
  ;; Escape Meta Alt Control Shift
  :custom ((initial-scratch-message "")
		   (inhibit-startup-message t)
		   (visible-bell t))
  :bind (("C-x <up>" . 'windmove-up)
		 ("C-x <down>" . 'windmove-down)
		 ("C-x <right>" . 'windmove-right)
		 ("C-x <left>" . 'windmove-left)
		 ("s-\\" . 'comment-or-uncomment-region))
  :hook ((prog-mode . display-line-numbers-mode)
		 (prog-mode . hs-minor-mode))
  :init (add-to-list 'package-archives
					 '("melpa" . "https://melpa.org/packages/") t)
  :config
  (menu-bar-mode 0)
  (when (display-graphic-p)
	(tool-bar-mode 0)
	(scroll-bar-mode 0))
  (defun display-startup-echo-area-message ()
	"Appear in the echo area when starting Emacs."
	(message "Emacs Rocks!")))

;; =============================================================================

(use-package zenburn-theme
  ;; A low contrast color theme for Emacs.
  :ensure t
  :init (load-theme 'zenburn t)
  :config
  (set-cursor-color "#7F9F7F") ; zenburn-green
  (set-fringe-mode '(1 . 1)) ; fringe-width
  (setf (cdr (assq 'continuation fringe-indicator-alist))
		'(nil nil)) ;; right-curly-arrow right indicator
  (set-face-attribute 'fringe nil ;; not working with :custom-face
					  :foreground (face-foreground 'default)
					  :background (face-background 'default))
  (cond ((eq system-type 'darwin)
		 (set-face-attribute 'default nil :font "menlo-14")
		 (setq-default line-spacing 2))
		((eq system-type 'gnu/linux)
		 (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
		 (setq-default line-spacing 1))))

;; =============================================================================

(use-package diminish
  ;; Diminished modes are minor modes with no modeline display
  :ensure t)

(use-package savehist
  ;; Saves minibuffer  history to an external file after exit
  :init (savehist-mode))

(use-package neotree
  ;; A tree plugin like NerdTree for Vim
  :ensure t
  :custom (neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package esup
  ;; Benchmark Emacs Startup time without ever leaving your Emacs.
  :ensure t :custom (esup-depth 0))

(use-package magit
  ;; A Git porcelain inside Emacs.
  :ensure t :defer t)

(use-package paren
  ;; Highlights matching parens
  :custom ((show-paren-style 'parenthesis)
		   (show-paren-when-point-in-periphery t)
		   (show-paren-when-point-inside-paren t))
  :custom-face (show-paren-match ((nil :background "pale turquoise"
									   :foreground "maroon4"
									   :weight ultra-bold)))
  :hook (after-init-hook . show-paren-mode))

(use-package beacon
  ;; Highlight the cursor whenever the window scrolls
  :ensure t
  :diminish beacon-mode
  :config (beacon-mode 1))

(use-package hideshow
  ;; https://stackoverflow.com/questions/1506764
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map ;; Key bindings only work in GUI
			  ("C-+" . 'hs-show-all) ;; ctrl+shift+=
			  ("C-_" . 'hs-hide-all)   ;; ctrl+shift+-
			  ("C-=" . 'hs-show-block)
			  ("C--" . 'hs-hide-block)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts "yes")))

(use-package all-the-icons-completion
  :ensure t
  :after all-the-icons
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package highlight-numbers
  ;; Highlight numbers in source code
  :ensure t
  :defer t
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  ;; Highlight TODO and similar keywords
  ;; TODO NEXT OKAY FAIL DONE TEMP XXX
  :ensure t
  :defer t
  :hook ((prog-mode LaTeX-mode) . hl-todo-mode))

(use-package vertico
  ;; VERTical Interactive COmpletion
  :ensure t
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package wc-mode
  ;; Running word count with goals (minor mode)
  ;; Essential for correct indent-for-tab-command in LaTeX-mode
  :ensure t
  :custom (wc-modeline-format "WC[%w %tw %tc]")
  :hook text-mode)

(use-package fill-column-indicator
  ;; Graphically indicate the fill column
  :ensure t
  :custom ((fci-rule-column 80)
		   (fci-rule-color "#4f4f4f"))
  :hook (prog-mode . fci-mode))

(use-package marginalia
  ;; Enrich existing commands with completion annotations
  :ensure t
  :custom-face (marginalia-documentation ((nil :inherit font-lock-doc-face)))
  :init (marginalia-mode))

(use-package writeroom-mode
  ;; Minor mode for distraction-free writing
  :ensure t
  :defer t
  :hook (writeroom-mode-hook . (lambda () (fci-mode 0)))
  :config
  (writeroom-adjust-width
   (truncate (- (* 0.52 (/ (display-pixel-width)
						   (frame-char-width)))
				visual-fill-column-width)))
  (setf (cdr (assq 'continuation fringe-indicator-alist))
		'(nil nil))) ;; no continuation indicators

(use-package move-text
  ;; Move current line or region with M-up or M-down.
  :ensure t :config (move-text-default-bindings))

(use-package smartparens
  ;; Automatic insertion, wrapping and paredit-like navigation with
  ;; user defined pairs.
  ;; For some weird reason, smartparens must be placed after move-text.
  :ensure t
  :diminish smartparens-mode
  :init (require 'smartparens-config)
  :config
  (smartparens-global-mode)
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\\[" "\\]")
  ;; Remove unnecessary pairs.
  (sp-local-pair 'org-mode "*" nil :actions :rem)
  (sp-local-pair 'org-mode "_" nil :actions :rem)
  (sp-local-pair 'org-mode "=" nil :actions :rem)
  (sp-local-pair 'org-mode "~" nil :actions :rem)
  (sp-local-pair 'org-mode "/" nil :actions :rem))

;; =============================================================================

(use-package tex
  ;; Integrated environment for *TeX*
  :ensure auctex
  :custom ((TeX-PDF-mode t)
		   (preview-scale-function 1.2)
		   (preview-auto-cache-preamble t))
  :hook (LaTeX-mode . (lambda ()
						(setq-local completion-at-point-functions
									(append '(cape-tex)
											completion-at-point-functions))))
  :config
  (with-eval-after-load "latex"
    (add-to-list 'LaTeX-verbatim-macros-with-braces "hphantom")
    (add-to-list 'LaTeX-verbatim-macros-with-braces "vphantom"))
  (defmacro my/tex-kbd (k pattern)
	`(add-hook 'LaTeX-mode-hook
			   (lambda ()
				 (local-set-key (kbd ,k)
								(lambda () (interactive)
								  (insert ,pattern) (backward-char))))))
  (my/tex-kbd "C-c a" "\\abra{}")
  (my/tex-kbd "C-c c" "\\abrc{}")
  (my/tex-kbd "C-c r" "\\abrr{}")
  (my/tex-kbd "C-c s" "\\abrs{}")
  (my/tex-kbd "^" "^{}")
  (my/tex-kbd "_" "_{}"))

(use-package pdf-tools
  ;; Support library for PDF documents
  :ensure t
  :if (display-graphic-p)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . pdf-view-themed-minor-mode)
		 (pdf-view-mode . (lambda ()
							(corfu-mode 0)
							(yas-minor-mode 0)
							(auto-revert-mode 1))))
  :config (pdf-tools-install t))

(use-package org
  ;; Your life in plain text
  :defer t
  :custom ((org-startup-truncated nil)
		   (org-adapt-indentation t))
  :bind (:map org-mode-map
			  ("C-c e" . (lambda () (interactive)
						   (progn (indent-for-tab-command)
								  (insert "#+BEGIN_EXPORT latex")
								  (newline-and-indent 2)
								  (insert "#+END_EXPORT")
								  (forward-line -1)
								  (indent-for-tab-command)
								  (org-edit-export-block))))))

;; =============================================================================

(use-package python
  ;; Python major mode
  :defer t
  :custom ((python-shell-interpreter "ipython")
		   (python-shell-interpreter-args "--simple-prompt --colors=Linux")
		   (python-indent 4)
		   (tab-width 4)))

(use-package markdown-mode
  ;; Major mode for Markdown-formatted text
  :ensure t :defer t)

(use-package csv-mode
  ;; Major mode for editing comma/char separated values
  :ensure t :defer t)

;; =============================================================================

(use-package corfu
  ;; COmpletion in Region FUnction
  :ensure t
  :custom ((corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
		   (corfu-auto t)  ;; Enable auto completion
		   (corfu-auto-delay 0.3)
		   (corfu-min-width 40)
		   (corfu-count 10)
		   (corfu-popupinfo-delay '(0.5 . 0.5))
		   (completion-cycle-threshold 3)
		   (tab-always-indent 'complete))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :init (global-corfu-mode))

(use-package corfu-terminal
  ;; Corfu popup on terminal
  :ensure t
  :unless (display-graphic-p)
  :config (corfu-terminal-mode +1))

(use-package kind-icon
  ;; Completion kind icons
  :ensure t
  :after corfu
  :custom (kind-icon-blend-background t)
  :hook (my-completion-ui-mode . (lambda ()
								   (setq completion-in-region-function
   										 (kind-icon-enhance-completion
   										  completion-in-region-function))))
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  ;; Completion At Point Extensions
  :ensure t
  :bind (("C-c p p" . completion-at-point))
  :hook (eglot-managed-mode . (lambda ()
								(setq-local completion-at-point-functions
											(list (cape-capf-super
												   #'eglot-completion-at-point
												   #'cape-dabbrev
												   #'cape-keyword
												   #'cape-file)
												  t))))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; cape-dabbrev has higher priority than cape-dict.
  ;; cape-dict activates after 4 characters
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history))

(use-package eglot
  ;; The Emacs Client for LSP servers
  :hook ((python-mode c++-mode LaTeX-mode) . eglot-ensure)
  :config (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab"))))

(use-package company
  ;; Modular text completion framework
  :ensure t)

(use-package yasnippet
  ;; Yet another snippet extension for Emacs
  :ensure t
  :hook (((prog-mode LaTeX-mode shell-mode) . yas-minor-mode)
		 (eglot-managed-mode
		  . (lambda ()
			  (setq-local completion-at-point-functions
						  (list (cape-capf-super
								 #'eglot-completion-at-point
								 (cape-company-to-capf #'company-yasnippet))
								t))))))
(use-package yasnippet-snippets
  ;;Collection of yasnippet snippets
  :after yasnippet
  :ensure t)

;; =============================================================================

(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold 1000000)))

;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zenburn-theme yasnippet-snippets writeroom-mode wc-mode vertico smartparens pdf-tools neotree move-text markdown-mode marginalia magit kind-icon hl-todo highlight-numbers fill-column-indicator exec-path-from-shell esup diminish csv-mode corfu-terminal company cape beacon auctex all-the-icons-completion)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
