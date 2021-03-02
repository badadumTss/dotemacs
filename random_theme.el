;; SPACELINE
(use-package spaceline
 :ensure t
 :config
 (require 'spaceline-config)
 (spaceline-helm-mode)
 (spaceline-spacemacs-theme))

(defvar lz/custom-theme-list
 nil
 "list of custom theme packages to install if not already
 installed at startup")

(setq lz/custom-theme-list
     '(material-theme 
	cyberpunk-theme 
	afternoon-theme 
	zenburn-theme 
	doom-themes
	flucui-themes 
	hemisu-theme 
	moe-theme))

(defvar lz/choosen-themes
 nil
 "List of *installed* themes to choose a random from")

;; nil per il tema di default
(setq lz/choosen-themes
     '(cyberpunk
	flucui-light
	flucui-dark
	leuven
	doom-Iosvkem
	doom-one-light
	doom-city-lights
	doom-dark+
	doom-material))

(dolist (theme lz/custom-theme-list)
 (unless (package-installed-p theme)
   (package-install theme)))

(defun disable-all-themes-in-list (themes-list)
 (dolist (theme themes-list)
   (disable-theme theme)))

(defun lz/random-theme ()
 "Disable all current themes and loads a new radom theme from
lz/choosen-themes"
 (disable-all-themes-in-list custom-enabled-themes)
 (load-theme 
  (nth 
   (random n
    (length lz/choosen-themes)) 
   lz/choosen-themes) 
  t))

;; Disabilita tutti i temi abilitati e carica uno randomico
;; selezionato da una lista specifica all'avvio di ogni frame
;; (add-hook 'before-make-frame-hook 'lz/random-theme)
