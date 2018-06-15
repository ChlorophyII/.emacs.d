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

;; To avoid a stupid bug of package-install caused by empty archive
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; =============================================================================

;; Warning: Don't touch the code below
;; AUCTeX will not function even a single character is missed

(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(require 'tex)
;; Notice the slash after "texbin"
(setenv "PATH" (concat
		"/usr/local/bin:"
		"/usr/local/sbin:"
		"/Library/TeX/texbin/:"
		(getenv "PATH")))
(setq LaTeX-electric-left-right-brace t)
;; To use AUCTeX preview, gs is needed
(cond ((eq system-type 'darwin)
       (setq preview-gs-command "/usr/local/bin/gs"))
      ((eq system-type 'gnu/linux)
       (setq preview-gs-command "/usr/bin/gs")))

(setq preview-auto-cache-preamble t)

;; =============================================================================

;; To autoload bash init file
(shell-command "source ~/.bashrc")

;; To enable or disable web development packages
(setq web-development nil)

;; ChezScheme is ChlorophyII's default option
(setq scheme-program-name "chez")

;; =============================================================================

(unless (package-installed-p 'matlab-mode)
  (package-install 'matlab-mode))

(when web-development
  (use-package web-mode ; Major mode for editing web templates
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)))

  (use-package sql-indent ; Indentation of SQL statements
    :ensure t
    :config
    (with-eval-after-load "sql"
      (load-library "sql-indent"))
    (setq sql-indent-offset 2))

  (use-package impatient-mode ; Serve buffers live over HTTP
    :ensure t
    :hook ((web-mode css-mode) . impatient-mode)))

(use-package flycheck ; On-the-fly syntax checking
  :ensure t
  :config
  (global-flycheck-mode t))

(use-package auto-complete ; Auto Completion for GNU Emacs
  :ensure t
  :hook (LaTeX-mode . auto-complete-mode)
  :config
  (global-auto-complete-mode t))

(use-package multiple-cursors ; Multiple cursors for Emacs
  :ensure t
  :hook ((prog-mode web-mode) . multiple-cursors-mode)
  :config
  (global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-S-p") 'mc/mark-previous-like-thismc)
  (global-set-key (kbd "C-c C-S-n") 'mc/mark-all-like-this))

(use-package neotree ; A tree plugin like NerdTree for Vim
  :ensure t)

(use-package markdown-mode ; Major mode for Markdown-formatted text
  :ensure t)

(use-package wc-mode ; Running word count with goals (minor mode)
  :ensure t
  :hook text-mode)

(use-package rainbow-mode ; Colorize color names in buffers
  :ensure t
  :hook css-mode)

(use-package highlight-parentheses ; Highlight surrounding parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode))

(use-package highlight-numbers ; Highlight numbers in source code
  :ensure t
  :hook ((prog-mode matlab-mode) . highlight-numbers-mode))

;; =============================================================================

(unless (package-installed-p 'zenburn-theme)
  (package-install 'zenburn-theme))
(load-theme 'zenburn t)

(cond ((eq system-type 'darwin)
       (set-face-attribute 'default nil :font "menlo-14")
       (setq-default line-spacing 2))
      ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
       (setq-default line-spacing 1)))

(setq-default cursor-type 'bar)

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

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message ()
  (message "Emacs Rocks!"))

;; wraps the lines in org-mode
(setq org-startup-truncated nil)

;; =============================================================================

(defun add-hook-list (mode-hook hook-list)
  (dolist (hook hook-list)
    (add-hook mode-hook hook)))

(add-hook-list 'prog-mode-hook
	       (list #'linum-mode
		     #'electric-pair-mode))

(add-hook-list 'matlab-mode-hook
	       (list #'linum-mode))

(add-hook-list 'LaTeX-mode-hook
	       (list #'linum-mode))

(when web-development
  (add-hook-list 'web-mode-hook
		 (list #'httpd-start))
  (add-hook-list 'css-mode-hook
		 (list #'httpd-start)))

;; =============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors zenburn-theme wc-mode rainbow-mode markdown-mode impatient-mode neotree highlight-parentheses auto-complete flycheck auctex use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here