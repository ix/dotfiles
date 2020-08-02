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
  (blink-cursor-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  :custom
  (cursor-type 'bar)
  (truncate-lines t)
  (vc-follow-symlinks t)
  (inhibit-compacting-font-caches t)
  (inhibit-startup-message t)
  (left-fringe-width 0)
  (right-fringe-width 0)
  (tags-add-tables nil)
  (initial-scratch-message ";; Present day... Present time!\n")
  (frame-title-format '(:eval (concat (current-dirname) "%b")))
  (tramp-default-method "rsync")
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)
   ("<f10>" . previous-buffer)
   ("<f12>" . next-buffer)))

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
  :bind (
	 ( "C-p" . projectile-find-file)
	 ( "C-S-p" . projectile-switch-to-buffer)
	 ( "C-t" . projectile-find-tag ))
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  (projectile-hg-command "hg files -0 -I .")
  :config
  (projectile-global-mode t))

(use-package multiple-cursors
  :bind (( "C-S-s" . mc/mark-all-like-this-dwim )
	 ( [C-mouse-1] . mc/add-cursor-on-click )
	 ( "C-<up>" . mc/mmlte--up )
	 ( "C-<down>" . mc/mmlte--down )))

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
(global-set-key (kbd "M-C-.") #'xref-find-definitions-friendly)
(global-unset-key [C-down-mouse-1])

;; GENERATED STUFF ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (ewal-doom-vibrant)))
 '(custom-safe-themes
   (quote
    ("dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" "174502267725776b47bdd2d220f035cae2c00c818765b138fea376b2cdc15eb6" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "2a749c20af891c16571527d07976bbcf2bf31819fa7d322942b73386019f4d58" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "34b3219ae11acd81b2bb7f3f360505019f17d7a486deb8bb9c1b6d13c6616d2e" "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1" "632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "00afb24bdd00626723d5af1e6725fc47f3aaa8e124c3c8acf62c1520f9889f81" "5b01f200e031fabf99b47628aaa3b94295acc393a3020cbd9249ea9a8b3b9c26" "0f35846313a8f0b3531487a34cce51e4f244375c4202921f25d2d18187930e51" "4eec00d4990ae4c92b448fd59561d11c387ad929a14d90deb8a3f1f400fd739f" "bff457e065647a79f45d65c18a5a746e27014ed8f25e61bda6e7a44e6986b416" "f4bca1c8f7ec70f03c861129aabb9ace3933925f9bfff23812accd46da3f303b" "d5d038918db2159bea34a436fccfe5d4e9d1aaa8f24cf9d536d163d365320971" "4cb43a33c9d8a7e8bd8cf859c280643842bbdcfe4bbe4b793b0953d41eeb1a7e" "ae223695581e7b8786087197a4fa1866302fd526fc83ac1cc143846511afb0fe" "f9c0d50b10229ae157371dc60e068c0d54156dae901c8a5eaba17154f1dc1cc9" "787248246e576797f36d224ef54bc352cf4239918089751654c01d26e0c4458e" "755873916e581760a37401ae60bde5fa3e0df7efefd98baee7684937219a2db6" "eabe50162e9fa5ee4a944173baecb6f68f9062dc0c9330ab79a5f9de546e518b" "ba368e28db95d8d96aafa87d30158ae37bd9463378f83e6874b09ad1f66b5e03" "1d28e7c6e962f3513c53d4f8f5dc64201ef2616e6753c777f7d90817fb4d4bf4" "febe60489adf5165bfe8a913073b656da81d1f752805a6c073982523e2a0e4d6" "75db1b229bad0a49926cf609e24ee9f9696a7c98838ef9d1dbf8f8d64ec3b80c" "14a6c8899f900c9a2cada85eb7663d107cc43b047e63ae61a72776bdfc430035" "4b07082cf5a68b765cb788d66239359c364e7e977465b210e2e13e1db7c31a1f" "4ae4fd557b8baffabeaf7bd579db8b200bb6d843ddc239fd509702ba9621e508" "b04146131ac02f57a52751c4beb5f7795c9e068a13f582b471afedf05b7014dd" "5b3fb853c645dbccdf3ddd1f4057800987948740b72ee1b14480afa2ee86eb9f" "ee59e88bba46950f8f47484633cf35d9bde1209b3db9127e8e076ce1b7d4fcee" "4b5c616d69e67864026daf34261104df8eb7489dbf391d8ba54937ef3cbbd8f4" "90d29100819231e868e01cc753cbab4f83b65ced36afb05539839f553789e11e" "a9796d416d67347362e2f559a0ba0f452de6b3b74bb35536e2b0477212585fd4" "4e43c3e90ac5e87051e3435cfe23871100abc54a1c98e1c937d398cc49194d34" "edd66bfa8a3ead12d67109a7a0016f0b31f32f2de717bbc8197c89fa5c329bb1" "92baf87c10e3d9502f1b7b2adc6af6ae44c91244e58485a3c2a156cb49173cff" "92b428fe21b7ddfe0e9ec9947b94b587e2d2e752048a6f6c738f89f60889c4d8" "32333d001577703b42ec1755b2304ddfe5ab2ed681fa119db6a24949b73f28c7" "86704574d397606ee1433af037c46611fb0a2787e8b6fd1d6c96361575be72d2" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" default)))
 '(display-battery-mode t)
 '(doom-modeline-height 0)
 '(doom-modeline-icon t)
 '(doom-modeline-mode t)
 '(doom-modeline-unicode-fallback t)
 '(ewal-doom-vibrant-brighter-modeline t)
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
 '(inhibit-startup-screen t)
 '(initial-scratch-message ";; Present day... Present time!

")
 '(ivy-wrap t)
 '(js-indent-level 2)
 '(left-fringe-width 0 t)
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(package-selected-packages
   (quote
    (glsl-mode solaire-mode eglot ox-confluence quelpa org-roam elm-mode ewal-doom-themes lua-mode ivy-xref auctex focus highlight-indent-guides badwolf-theme github-modern-theme leuven-theme sorcery-theme ivy projectile emojify w3m circadian xresources-theme shakespeare-mode ample-theme treemacs srcery-theme slim-mode haml-mode darktooth-theme rainbow-mode markdown-mode+ yaml-mode doom doom-themes doom-modeline dracula-theme vue-mode restart-emacs yafolding magit monky multiple-cursors monokai-pro-theme haskell-mode diminish use-package twilight-bright-theme twilight-anti-bright-theme swiper rg projectile-ripgrep molokai-theme kaolin-themes git-gutter centered-window)))
 '(projectile-completion-system (quote ivy))
 '(projectile-hg-command "hg files -0 -I .")
 '(projectile-indexing-method (quote alien))
 '(right-fringe-width 0 t)
 '(standard-indent 2)
 '(tags-add-tables nil)
 '(tool-bar-mode nil)
 '(tramp-inline-compress-start-size 1000000)
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
