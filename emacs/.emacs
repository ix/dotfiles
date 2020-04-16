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
  (global-prettify-symbols-mode t)
  (global-auto-revert-mode t)
  (xterm-mouse-mode)
  (display-battery-mode t)
  :custom
  (truncate-lines t)
  (inhibit-compacting-font-caches t)
  (inhibit-startup-message t)
  (left-fringe-width 0)
  (right-fringe-width 0)
  (tags-add-tables nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (initial-scratch-message ";; Present day... Present time!\n")
  (frame-title-format '(:eval (concat (current-dirname) "%b"))))

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(custom-enabled-themes (quote (monokai-pro)))
 '(custom-safe-themes
   (quote
    ("174502267725776b47bdd2d220f035cae2c00c818765b138fea376b2cdc15eb6" "0daf22a3438a9c0998c777a771f23435c12a1d8844969a28f75820dd71ff64e1" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" "63df625509cec7b2b7deba1d48ef07ffbee99c735a95d130ad9d1fc3df8b9a34" "d54834a3ec381644880b49fb82ce9d6ccc11997510c7071cfbf3558c8f4b68f6" "72085337718a3a9b4a7d8857079aa1144ea42d07a4a7696f86627e46ac52f50b" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "55c2069e99ea18e4751bd5331b245a2752a808e91e09ccec16eb25dadbe06354" "983eb22dae24cab2ce86ac26700accbf615a3f41fef164085d829fe0bcd3c236" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "f0a76ae259b7be77e59f98501957eb45a10af0839dd9eb29fdd5691ed74771d4" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "2f945b8cbfdd750aeb82c8afb3753ebf76a1c30c2b368d9d1f13ca3cc674c7bc" "3de8a8c40fdb33dbe7ef1d6706507ef94346d46c0b0c340c5877a80bb0d762db" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "88049c35e4a6cedd4437ff6b093230b687d8a1fb65408ef17bfcf9b7338734f6" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "07e3a1323eb29844e0de052b05e21e03ae2f55695c11f5d68d61fb5fed722dd2" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "72fda75af7caddec17ba9b49d2f99703c20a5f5f5c4dcec641d34a0b83569e88" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "d5d2ab76985738c142adbe6a35dc51c8d15baf612fdf6745c901856457650314" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "a038af4fff7330f27f4baec145ef142f8ea208648e65a4b0eac3601763598665" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "d1c7f2db070c96aa674f1d61403b4da1fff2154163e9be76ce51824ed5ca709c" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "c520bbbddca1d7362d046635c5cc023b5f151b250ac9f8d6ce763afa804b7d1d" "559b28ae6deb74713fee9064e7ece54cb71ba645f44acbf81ad7916a4f947815" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "58c2c8cc4473c5973e77f4b78a68c0978e68f1ddeb7a1eb34456fce8450be497" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" "179ff455fbab61b1c5be8da791c53c4a2b65598dc372031be1e95373bd9a1f25" "3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" "09cadcc2784baa744c6a7c5ebf2a30df59c275414768b0719b800cabd8d1b842" "f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "8dce5b23232d0a490f16d62112d3abff6babeef86ae3853241a85856f9b0a6e7" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" default)))
 '(display-battery-mode t)
 '(doom-modeline-height 0)
 '(doom-modeline-icon t)
 '(doom-modeline-mode t)
 '(doom-modeline-unicode-fallback t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(frame-title-format (quote (:eval (concat (current-dirname) "%b"))) t)
 '(fset (quote yes-or-no-p) t)
 '(git-gutter:added-sign "▊")
 '(git-gutter:deleted-sign "▊")
 '(git-gutter:handled-backends (quote (git hg)))
 '(git-gutter:handled-bckends (quote (git hg)) t)
 '(git-gutter:modified-sign "▊")
 '(global-display-line-numbers-mode t)
 '(global-git-gutter-mode t)
 '(hl-sexp-background-color "#33323e")
 '(inhibit-compacting-font-caches t t)
 '(inhibit-startup-message t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message ";; Present day... Present time!\n\n")
 '(ivy-wrap t)
 '(js-indent-level 2)
 '(left-fringe-width 0 t)
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(nrepl-message-colors
   (quote
    ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(package-selected-packages
   (quote
    (focus highlight-indent-guides badwolf-theme github-modern-theme leuven-theme sorcery-theme ivy projectile emojify w3m circadian xresources-theme shakespeare-mode ample-theme treemacs srcery-theme slim-mode haml-mode darktooth-theme rainbow-mode markdown-mode+ yaml-mode doom doom-themes doom-modeline dracula-theme vue-mode restart-emacs yafolding magit monky multiple-cursors monokai-pro-theme haskell-mode diminish use-package twilight-bright-theme twilight-anti-bright-theme swiper rg projectile-ripgrep molokai-theme kaolin-themes git-gutter centered-window)))
 '(pdf-view-midnight-colors (quote ("#dddddd" . "#000000")))
 '(projectile-completion-system (quote ivy))
 '(projectile-hg-command "hg files -0 -I .")
 '(projectile-indexing-method (quote alien))
 '(right-fringe-width 0 t)
 '(tags-add-tables nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (quote
    ((20 . "#6a737d")
     (40 . "#032f62")
     (60 . "#6a737d")
     (80 . "#6a737d")
     (100 . "#6a737d")
     (120 . "#d73a49")
     (140 . "#6a737d")
     (160 . "#6a737d")
     (180 . "#6a737d")
     (200 . "#6a737d")
     (220 . "#22863a")
     (240 . "#005cc5")
     (260 . "#6f42c1")
     (280 . "#6a737d")
     (300 . "#005cc5")
     (320 . "#6a737d")
     (340 . "#d73a49")
     (360 . "#6a737d"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "CozetteVector" :foundry "????" :slant normal :weight normal :height 90 :width normal))))
 '(mmm-code-submode-face ((t nil)))
 '(mmm-default-submode-face ((t nil))))
