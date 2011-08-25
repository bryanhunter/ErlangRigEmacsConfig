;; Explorer more popular emacs options here: http://www.emacswiki.org/emacs/PopularOptions

(setq default-directory "C:/code/" )

;; color-theme (http://www.emacswiki.org/emacs/ColorTheme)
(add-to-list 'load-path "~/emacs-config/color-theme")
(require 'color-theme)
	(color-theme-initialize)
	;;	(color-theme-calm-forest)
	;;	(color-theme-goldenrod)
	;;	(color-theme-robin-hood)
	;;	(color-theme-gnome2)
	;; 	(color-theme-ld-dark)
	(color-theme-clarity)

;; 	The value is in 1/10pt, so 100 will give you 10pt, etc.
(custom-set-faces
	'(default ((t (
		:inherit nil 
		:stipple nil 
		;;	:background "black" 
		;;	:foreground "white" 
		;; 	:inverse-video nil 
		;; 	:box nil 
		:strike-through nil 
		:overline nil 
		:underline nil 
		:slant normal 
		:weight normal
		:height 140 	;; The value is in 1/10pt, so 100 will give you 10pt, etc.
		:width normal
		:foundry "unknown" 
		:family "Consolas"
		;; :family "DejaVu Sans" ;; You can download this lovely font here: http://dejavu-fonts.org/wiki/Download
	)))))

(require 'dircolors)

;;set cursor colour
(set-cursor-color "yellow")

;;make sure ansi colour character escapes are honored
(ansi-color-for-comint-mode-on)

;;highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")

(require 'smooth-scrolling)
	
;; Display the current row and column number at the bottom of the window
(line-number-mode 1)
(column-number-mode 1)

;; Hide the hideous Emacs splash screen	 
(setq inhibit-splash-screen t)						

;; Set up buffer switching to mimic Visual Studio
(require 'cycle-buffer)
(global-set-key [C-S-tab] 'cycle-buffer-backward)
(global-set-key [C-tab] 'cycle-buffer)
(global-set-key [M-f4] (lambda () (interactive) (kill-buffer nil)))

(setq read-file-name-completion-ignore-case nil)

;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)

(setq make-backup-files nil)			;; More here http://www.emacswiki.org/emacs/BackupDirectory
(setq use-file-dialog nil)

(require 'pc-select) 					;; Make copy mouse selection work in the usual Mac/Windows way
(transient-mark-mode t) 				;; highlight text selection
(delete-selection-mode t) 				;; delete seleted text when typing
(cua-mode t) 							;; windows style keybind C-x, C-v, cut paste
(setq cua-auto-tabify-rectangles nil) 	;; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t) 	;; Selection remains after C-c
	 
;; ido provides a very nice auto-complete for finding files (type C-x f)
;; Learn more here: http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)
	  
;; programming conveniences:
(show-paren-mode t) ; light-up matching parens
(global-font-lock-mode t) ; turn on syntax highlight
(setq text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))

;get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Add language configurations
(require 'erlang-config)
(require 'fsharp-config)
(require 'csharp-config)

;; Add some Git goodies
(setq load-path (cons "~/emacs-config/magit" load-path))
(require 'magit)
 
(provide 'my-config)
