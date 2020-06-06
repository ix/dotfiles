(package-initialize)
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

;; HELPER FUNCTIONS ;;
(defconst *dirsep* "/")

(defun current-dirname ()
  (if (eq nil buffer-file-name) ""
    (concat "[" (car (last (split-string buffer-file-truename *dirsep*) 2)) "] ")))

(defun generate-guid ()
  (interactive)
  (insert
   (format "idFromWords 0x%04x%04x 0x%04x%04x 0x%04x%04x 0x%04x%04x"
	   (abs (random (expt 16 4)))
	   (abs (random (expt 16 4)))
	   (abs (random (expt 16 4)))
	   (abs (random (expt 16 4)))
	   (abs (random (expt 16 4)))
	   (abs (random (expt 16 4)))
	   (abs (random (expt 16 4)))
	   (abs (random (expt 16 4))))))

(defun haskell-insert-language-pragma ()                                                                  
  (interactive)                                                                                           
  (let ((entries '( "OverloadedStrings"                                                                   
		    "DeriveGeneric"                                                                       
		    "DeriveAnyClass"                                                                      
		    "RecordWildCards"                                                                     
		    "RankNTypes"                                                                          
		    "TupleSections"                                                                       
		    "LambdaCase"                                                                          
		    "TemplateHaskell"                                                                     
		    "BinaryLiterals"                                                                      
		    "MultiWayIf"                                                                          
		    "OverloadedLists"                                                                     
		    "TypeOperators"                                                                       
		    "MultiParamTypeClasses")))                                                            
    (insert (concat "{-# LANGUAGE " (message "%s" (ivy-completing-read "Select language pragma: " entries)) " #-}"))))

(use-package emacs
  :init
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (delete-selection-mode t)
  (global-prettify-symbols-mode t)
  (global-auto-revert-mode t)
  (xterm-mouse-mode)
  (display-battery-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  :custom
  (truncate-lines t)
  (vc-follow-symlinks t)
  (inhibit-compacting-font-caches t)
  (inhibit-startup-message t)
  (left-fringe-width 0)
  (right-fringe-width 0)
  (tags-add-tables nil)
  (initial-scratch-message ";; Present day... Present time!\n")
  (frame-title-format '(:eval (concat (current-dirname) "%b")))
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)))

(use-package haskell-mode
  :custom
  (haskell-process-type (quote stack-ghci))
  (haskell-font-lock-quasi-quote-modes
   `( ("julius"  . shakespeare-julius-mode)
      ("hamlet"  . shakespeare-hamlet-mode)
      ("whamlet" . shakespeare-hamlet-mode)
      ("shamlet" . shakespeare-hamlet-mode)
      ("lucius" . shakespeare-lucius-mode)
      ("glsl" . glsl-mode) )))

(use-package vue-mode
  :mode "\\.vue$"
  :config
  (add-to-list 'mmm-save-local-variables '(syntax-ppss-table buffer)))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character))

(use-package projectile
  :bind (( "C-p" . projectile-find-file))
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  (projectile-hg-command "hg files -0 -I .")
  :config
  (projectile-global-mode t))

(use-package ivy
  :custom
  (ivy-wrap t)
  :config
  (ivy-mode t))

(use-package ivy-xref
  :ensure t
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package ivy-posframe
;;   :custom
;;   (ivy-posframe-style 'frame-top-center)
;;   :config
;;   (ivy-posframe-mode))

(use-package git-gutter
  :custom
  (git-gutter:added-sign "❚")
  (git-gutter:deleted-sign "❚")
  (git-gutter:modified-sign "❚")
  (git-gutter:handled-bckends '(git hg))
  :config
  (global-git-gutter-mode t))

(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-PDF-mode t))
  
;; CUSTOM KEYBINDS ;;
(global-set-key (kbd "C-S-p") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-t") 'projectile-find-tag)
(global-set-key (kbd "M-C-.") #'xref-find-definitions-friendly)
(global-set-key (kbd "<f10>") 'previous-buffer)
(global-set-key (kbd "<f12>") 'next-buffer)
(global-unset-key [C-down-mouse-1])
(global-set-key [C-mouse-1] 'mc/add-cursor-on-click)
(global-set-key (kbd "C-S-s") 'mc/mark-all-like-this-dwim)

;; GENERATED STUFF ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (ewal-doom-one)))
 '(custom-safe-themes
   (quote
    ("8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" default)))
 '(display-battery-mode t)
 '(doom-modeline-height 0)
 '(doom-modeline-icon t)
 '(doom-modeline-mode t)
 '(doom-modeline-unicode-fallback t)
 '(frame-title-format (quote (:eval (concat (current-dirname) "%b"))) t)
 '(fset (quote yes-or-no-p) t)
 '(git-gutter:added-sign "▊")
 '(git-gutter:deleted-sign "▊")
 '(git-gutter:handled-backends (quote (git hg)))
 '(git-gutter:handled-bckends (quote (git hg)) t)
 '(git-gutter:modified-sign "▊")
 '(global-display-line-numbers-mode t)
 '(global-git-gutter-mode t)
 '(haskell-font-lock-quasi-quote-modes
   (quote
    (("julius" . shakespeare-julius-mode)
     ("hamlet" . shakespeare-hamlet-mode)
     ("whamlet" . shakespeare-hamlet-mode)
     ("shamlet" . shakespeare-hamlet-mode)
     ("lucius" . shakespeare-lucius-mode)
     ("glsl" . glsl-mode))))
 '(haskell-process-type (quote stack-ghci))
 '(highlight-indent-guides-method (quote character))
 '(inhibit-compacting-font-caches t t)
 '(inhibit-startup-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message ";; Present day... Present time!

")
 '(ivy-wrap t)
 '(js-indent-level 2)
 '(left-fringe-width 0 t)
 '(package-selected-packages
   (quote
    (elm-mode ewal-doom-themes lua-mode ivy-xref auctex focus highlight-indent-guides badwolf-theme github-modern-theme leuven-theme sorcery-theme ivy projectile emojify w3m circadian xresources-theme shakespeare-mode ample-theme treemacs srcery-theme slim-mode haml-mode darktooth-theme rainbow-mode markdown-mode+ yaml-mode doom doom-themes doom-modeline dracula-theme vue-mode restart-emacs yafolding magit monky multiple-cursors monokai-pro-theme haskell-mode diminish use-package twilight-bright-theme twilight-anti-bright-theme swiper rg projectile-ripgrep molokai-theme kaolin-themes git-gutter centered-window)))
 '(projectile-completion-system (quote ivy) t)
 '(projectile-hg-command "hg files -0 -I ." t)
 '(projectile-indexing-method (quote alien) t)
 '(right-fringe-width 0 t)
 '(standard-indent 2)
 '(tags-add-tables nil)
 '(tool-bar-mode nil)
 '(tramp-inline-compress-start-size nil)
 '(truncate-lines t)
 '(vc-follow-symlinks t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :width normal :family "CozetteVector"))))
 '(mmm-code-submode-face ((t nil)))
 '(mmm-default-submode-face ((t nil))))
