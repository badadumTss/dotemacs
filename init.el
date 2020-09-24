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

(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(global-set-key (kbd "C-c e") '(lambda () (interactive) (find-file 
							 (expand-file-name 
							  (concat user-emacs-directory "init.el")))))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil)
             '(horizontal-scroll-bars . nil))

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
(setq c-default-style
      (list '(java-mode . "java")
            '(awk-mode . "awk")
            '(other . "k&r")))

;; Associazione (nome-paccheto-tema nome-tema) così da usarlo in più punti
(setq lz/custom-theme-list
      '((modus-operandi-theme modus-operandi)
	(material-theme material)
	(cyberpunk-theme cyberpunk)
	(afternoon-theme afternoon)
	(zenburn-theme zenburn)
	(moe-theme (moe-light moe-dark))))

(dolist (theme lz/custom-theme-list)
  (unless (package-installed-p (car theme))
    (package-install theme)))

;; Disabilita tutti i temi presenti nella lista themes-list
(defun disable-all-themes-in-list (themes-list)
  "Disable all variable and face settings defined by THEMES-LIST.
See `custom-enabled-themes' for a list of enabled themes."
  (dolist (theme themes-list)
    (when (custom-theme-enabled-p theme)
      (let ((settings (get theme 'theme-settings)))
        (dolist (s settings)
          (let* ((prop   (car s))
                 (symbol (cadr s))
                 (val (assq-delete-all theme (get symbol prop))))
            (put symbol prop val)
            (cond
             ((eq prop 'theme-value)
              (custom-theme-recalc-variable symbol))
             ((eq prop 'theme-face)
              ;; If the face spec specified by this theme is in the
              ;; saved-face property, reset that property.
              (when (equal (nth 3 s) (get symbol 'saved-face))
                (put symbol 'saved-face (and val (cadr (car val)))))))))
        ;; Recompute faces on all frames.
        (dolist (frame (frame-list))
          ;; We must reset the fg and bg color frame parameters, or
          ;; `face-set-after-frame-default' will use the existing
          ;; parameters, which could be from the disabled theme.
          (set-frame-parameter frame 'background-color
                               (custom--frame-color-default
                                frame :background "background" "Background"
                                "unspecified-bg" "white"))
          (set-frame-parameter frame 'foreground-color
                               (custom--frame-color-default
                                frame :foreground "foreground" "Foreground"
                                "unspecified-fg" "black"))
          (face-set-after-frame-default frame))
        (setq custom-enabled-themes
              (delq theme custom-enabled-themes))))))

;; Temi tra i quali scegliere quello randomico
(setq choosen-themes
      (mapcan
       '(lambda (theme-obj)
	  (cdr theme-obj))
       lz/custom-theme-list))

;; Disabilita tutti i temi abilitati e carica uno randomico
;; selezionato da una lista specifica all'avvio di ogni frame
(add-hook 'before-make-frame-hook
          (lambda ()
            (disable-all-themes-in-list custom-enabled-themes)
            (load-theme
             (nth 
              (random 
               (length choosen-themes))
              choosen-themes)
             t )))

;; PACCHETTI AGGIUNTIVI
;; EVIL
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  
  (defun wrap-with-char (beg end open-char &optional close-char)
    (goto-char beg)
    (insert open-char)
    (goto-char (1+ end))
    (insert 
     (if close-char 
	 close-char 
       open-char)))

  (evil-define-operator wrap-with-brackets (beg end)
    (wrap-with-char beg end "(" ")"))
  
  (evil-define-operator wrap-with-quotes (beg end)
    (wrap-with-char beg end "'"))
  
  (evil-define-operator wrap-with-double-quotes (beg end)
    (wrap-with-char beg end "\""))
  
  (evil-define-operator wrap-with-square-brackets (beg end)
    (wrap-with-char beg end "[" "]"))
  
  (evil-define-operator wrap-with-curly-brackets (beg end)
    (wrap-with-char beg end "{" "}"))
  
  (evil-define-operator wrap-with-comment-region (beg end)
    (wrap-with-char beg end "/*" "*/"))
  
  (evil-define-key 'visual global-map (kbd "(") 'wrap-with-brackets)
  (evil-define-key 'visual global-map (kbd "'") 'wrap-with-quotes)
  (evil-define-key 'visual global-map (kbd "\"") 'wrap-with-double-quotes)
  (evil-define-key 'visual global-map (kbd "[") 'wrap-with-square-brackets)
  (evil-define-key 'visual global-map (kbd "{") 'wrap-with-curly-brackets)
  (evil-define-key 'visual global-map (kbd "/*") 'wrap-with-comment-region))


(use-package magit
  :ensure t)

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
  (define-key helm-map (kbd "C-z") #'helm-select-action))

(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key (kbd "M-S") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c C-s") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-s") 'helm-multi-swoop-all)

  (define-key helm-swoop-map (kbd "C-s") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key evil-motion-state-map (kbd "C-c C-s") 'helm-swoop-from-evil-search)

  (define-key helm-swoop-map (kbd "C-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  (define-key helm-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-j") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-j") 'helm-next-line)

  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color t
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match t))

(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (which-key-mode))

(use-package org
  :ensure t
  :config
  (setq org-startup-indented t
	org-hide-leading-stars t))

(use-package org-tree-slide
  :ensure t
  :config
  (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
  (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))

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

(use-package expand-region
  :ensure t
  :config
  (define-key evil-visual-state-map (kbd "e") 'er/expand-region))
