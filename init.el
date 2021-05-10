(setq user-email-address "lucazanny@gmail.com")

(setq inhibit-startup-buffer-menu nil)
(setq inhibit-startup-screen t)

(setq custom-file (concat user-emacs-directory "custom.el"))

;; BASE
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-hl-line-mode 1)

(if (daemonp)
    (global-set-key (kbd "C-x C-c") 'delete-frame)
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "https" "http")))
  (when no-ssl (warn "No ssl! MITM possibili!"))

  (add-to-list 'package-archives 
               (cons 
                "melpa" (concat proto "://melpa.org/packages/")) 
               t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun lz/open-configs ()
  "Funzione per aprire il file di configurazione"
  (interactive)
  (find-file 
   (expand-file-name 
    (concat user-emacs-directory "init.el"))))

;; prima della 26.0 di emacs non veniva fornita la
;; global-display-line-numbers, ci si affidava alla linum, che era più
;; lenta e non permetteva di avere il numero relativo di linea
(if (not (version< emacs-version "26.0"))
    (progn
      (global-display-line-numbers-mode t)
      (setq display-line-numbers-type 'relative))
  (global-linum-mode t)
  )

(tool-bar-mode -1)
(toggle-scroll-bar -1)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "C-x w") 'newsticker-show-news)
(setq newsticker-automatically-mark-items-as-old t)
(setq newsticker-url-list (list
                           '("XKCD" "https://xkcd.com/rss.xml" nil 3600)
                           '("Mike Zamansky" "https://cestlaz.github.io/rss.xml" nil 3600)
                           '("Luke Smiths" "https://lukesmith.xyz/rss.xml" nil 3600)))

;; TEMI - VISUAL
(if (not (equal system-type 'windows-nt))
    (progn
      (setq initial-frame-alist
	    '((width . 500) ; chars
	      (height . 200) ; lines
	      (left . 50)
	      (top . 50)
	      (vertical-scroll-bars . nil)
	      (horizontal-scroll-bars . nil)))

      (setq default-frame-alist
	    '((width . 500)
	      (height . 200)
	      (left . 50)
	      (top . 50)
	      (vertical-scroll-bars . nil)
	      (horizontal-scroll-bars . nil))))
  (progn
    (setq initial-frame-alist
	  '((vertical-scroll-bars . nil)
	    (horizontal-scroll-bars . nil)))

    (setq default-frame-alist
	  '((vertical-scroll-bars . nil)
	    (horizontal-scroll-bars . nil)))))

(set-face-attribute 'default nil :height 150)

;; PAREN MODE, shows parenthesis if visible otherwhise shows the
;; entire expression
(show-paren-mode 1)
(setq show-paren-style 'mixed)

(setq c-default-style
      (list '(java-mode . "java")
            '(awk-mode . "awk")
            '(other . "k&r")))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

;; MAGIT
(use-package magit
  :ensure t
  :init
  (setq load-path 
	(delq "/usr/share/emacs/25.2/site-lisp/elpa/magit-2.11.0" load-path)))

(use-package magit-gitflow
  :ensure t
  :config
  (require 'magit-gitflow)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; HELM
(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-b") #'helm-buffers-list)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  
  ;; (define-key helm-map (kbd "C-k") 'helm-previous-line)
  ;; (define-key helm-map (kbd "C-j") 'helm-next-line)

  (setq helm-multi-edit-save t)
  (setq helm-split-with-multiple-windows t)
  (setq helm-split-direction 'split-window-vertically)
  (setq helm-speed-or-color t)
  (setq helm-move-to-line-cycle t)
  (setq helm-use-line-number-face t)
  (setq helm-use-fuzzy-match t))

;; WHICH-KEY
(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (which-key-mode))

;; ORG
(use-package org
  :ensure t
  :config
  (setq	org-hide-leading-stars t)
  (setq org-startup-truncated nil)
  (require 'ox-latex)
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-minted-langs '(ipython "python"))
  (add-to-list 'org-latex-minted-langs '(java "java"))
  (require 'ox-md nil t)
  (add-hook 'org-mode-hook #'toggle-word-wrap))

(use-package org-tree-slide
  :ensure t
  :config
  (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
  (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))

(use-package expand-region
  :ensure t
  :config
  (require 'org)
  (global-set-key (kbd "C-c C-SPC") 'er/expand-region)) 

;; RUST
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))


;; WEB-DEV
(use-package web-mode
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; UTILITY
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (defun lz/java-def-var (var-name)
    "Defines the variable `var-name` as a String"
    (concat "public String " var-name ";"))
  (defun lz/java-init-var (var-name)
    "Defines the variable `var-name` as a String"
    (concat "this." var-name " = " var-name))
  (defun lz/org-get-time-stamp (&rest args)
    "Return the string that `org-insert-time-stamp' would insert."
    (with-temp-buffer
      (apply #'org-insert-time-stamp args)
      (buffer-string))))

(use-package debbugs
  :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; HASKELL
(use-package haskell-mode
  :ensure t)

;; GROOVY
(use-package groovy-mode
  :ensure t)

(use-package gradle-mode
  :ensure t)

;; SOLIDITY
(use-package solidity-mode
  :ensure t)

;; TYPESCRIPT
(use-package typescript-mode
  :ensure t)

;; JSON
(use-package json-mode
  :ensure t)

;; DOCKER
(use-package dockerfile-mode
  :ensure t)
