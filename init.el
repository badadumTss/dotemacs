(setq user-email-address "lucazanny@gmail.com")

(setq inhibit-startup-buffer-menu nil)
(setq inhibit-startup-screen t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(setq load-path (cons user-emacs-directory load-path)) ;; Adjust
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
  "Funzione per aprire il file di configurazione (questo)"
  (interactive)
  (find-file 
   (expand-file-name 
    (concat user-emacs-directory "init.el"))))

;; (global-set-key (kbd "C-c C-e") 'lz/open-configs)

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

(setq c-default-style
      (list '(java-mode . "java")
            '(awk-mode . "awk")
            '(other . "k&r")))


(load-theme 'modus-operandi t)

;; PACCHETTI AGGIUNTIVI
;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-C-u-scroll t)
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
;;   ;; (setq evil-insert-state-cursor '(box "purple"))
;;   ;; (setq evil-normal-state-cursor '(box "black"))
;;   (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
;;   (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  
;;   (defun wrap-with-char (beg end open-char &optional close-char)
;;     (goto-char beg)
;;     (insert open-char)
;;     (goto-char (1+ end))
;;     (insert 
;;      (if close-char 
;; 	 close-char 
;;        open-char))
;;     (goto-char beg)
;;     (evil-insert 1))
  
;;   (evil-define-operator wrap-with-brackets (beg end)
;;     (wrap-with-char beg end "(" ")"))
;;   (evil-define-operator wrap-with-quotes (beg end)
;;     (wrap-with-char beg end "'"))
;;   (evil-define-operator wrap-with-double-quotes (beg end)
;;     (wrap-with-char beg end "\""))
;;   (evil-define-operator wrap-with-square-brackets (beg end)
;;     (wrap-with-char beg end "[" "]"))
;;   (evil-define-operator wrap-with-curly-brackets (beg end)
;;     (wrap-with-char beg end "{" "}"))
;;   (evil-define-operator wrap-with-comment-region (beg end)
;;     (wrap-with-char beg end "/*" "*/"))
  
;;   (evil-define-key 'visual global-map (kbd "(") 'wrap-with-brackets)
;;   (evil-define-key 'visual global-map (kbd "'") 'wrap-with-quotes)
;;   (evil-define-key 'visual global-map (kbd "\"") 'wrap-with-double-quotes)
;;   (evil-define-key 'visual global-map (kbd "[") 'wrap-with-square-brackets)
;;   (evil-define-key 'visual global-map (kbd "{") 'wrap-with-curly-brackets)
;;   (evil-define-key 'visual global-map (kbd "/*") 'wrap-with-comment-region))

;; (use-package evil-magit
;;   :ensure t
;;   :config
;;   (setq evil-magit-use-y-for-yank t))

(use-package expand-region
  :ensure t
  :config
  ;; (define-key evil-visual-state-map (kbd "e") 'er/expand-region))
  (global-set-key (kbd "C-c C-SPC") 'er/expand-region)) 

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
  
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-j") 'helm-next-line)

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
  (setq	org-hide-leading-stars t))

(use-package org-tree-slide
  :ensure t
  :config
  (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
  (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))

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
    (concat "this." var-name " = " var-name)))

(use-package debbugs
  :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(setq auto-mode-alist
      (cons '("\\.mod$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.dat$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.ampl$" . ampl-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("ampl" . ampl-mode)
            interpreter-mode-alist))
(autoload 'ampl-mode "ampl-mode" "Ampl editing mode." t)
(put 'scroll-left 'disabled nil)
