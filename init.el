;; ;; Install necessary packages for emacs

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/setting/install-packages.el")
;; ;; Customer functions and configuration
(load "~/.emacs.d/setting/custom-functions.el")
(setq inhibit-startup-message t)
(load-theme 'material t)
(global-linum-mode t)
;; ;;; clear the console in R
(defun clear-shell ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

(global-set-key (kbd "\C-x c") 'clear-shell)





;; ;; Editing Configuration

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-smartparens pdf-tools auctex multi-term fix-word discover-my-major linum-relative highlight-symbol flycheck aggressive-indent ace-window imenu-anywhere helm-swoop goto-last-change elscreen tabbar evil-avy multiple-cursors window-numbering smex smartparens rainbow-delimiters r-autoyas org-plus-contrib material-theme key-chord js2-mode jinja2-mode helm evil-magit el-autoyas clojure-snippets better-defaults auto-complete ace-jump-buffer ace-flyspell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; elpy configuration
(elpy-enable)

;; ;; ido-mode configuration
(require 'ido)
(ido-mode t)

;; ;; Evil mode configuration
(require 'evil)
(add-to-list 'evil-emacs-state-modes 'nav-mode)
(evil-mode 1)
(setq evil-want-fine-undo 'fine)

;; ;; key-chord configuration
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
(key-chord-define-global "ji" 'other-window)
(key-chord-define-global "JJ" 'save-buffer)
(key-chord-define-global "KK" 'kill-buffer)
(key-chord-define-global "kl" 'switch-to-buffer)
(key-chord-define-global "LL" (lookup-key (current-global-map) (kbd "C-x C-f")))
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)




;; ;; ESS-configuration
(setq ess-use-tracebug t)
(setq ess-tracebug-prefix "\M-c")


;; ;; Aspell feature
(setq-default ispell-program-name "aspell")
;; (ac-flyspell-workaround)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker" t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command" t)

;; ;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; ;; better-defaults






;; ;; preserve everything after closing
(desktop-save-mode 1)
(setq history-length 500)
(add-to-list 'desktop-globals-to-save 'file-name-history)
   (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	        "\\)$"))
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


;; ;; smex
(require 'smex)

(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")

(global-set-key (kbd "M-x") 'smex)
(defadvice smex (around space-inserts-hyphen activate compile)
   (let ((ido-cannot-complete-command 
          `(lambda ()
             (interactive)
             (if (string= " " (this-command-keys))
                 (insert ?-)
               (funcall ,ido-cannot-complete-command)))))
     ad-do-it))

;; ;; auto-complete
(load "~/.emacs.d/setting/auto-complete-settings.el")
(load "~/.emacs.d/freq-word.el")
;; (ac-define-dictionary-source ac-freq-word freq-word-list)

;; ;; elpy
(when (require 'elpy nil t)
  (elpy-enable))
(setq elpy-rpc-backend "jedi")
;;;;sudo pip install elpy jedi rope
;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)
;; (setenv "PYTHONPATH" "/usr/bin/python")
(add-hook 'python-mode-hook '(lambda ()
			    (local-set-key (kbd "C-c C-a") 'elpy-autopep8-fix-code)))

;; ;; org
;; ;; org-plus-contrib
;; ;; rainbow-delimiters
;; ;; jinja2-mode
;; ;; yasnippet
(require 'yasnippet)
(yas/initialize)
;; (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(setq yas-snippet-dirs '("~/.emacs.d/yasnippet-snippets"))
(yas-global-mode t)
;; (set-default 'ac-sources
;;              '(ac-source-abbrev
;;                ac-source-dictionary
;;                ac-source-yasnippet
;;                ac-source-words-in-buffer
;; 	       ac-source-files-in-current-dir
;; 	       ac-source-words-in-same-mode-buffers
;; ;;               ac-source-semantic
;; 	       ac-freq-word
;; 	       ))




;; ;; r-autoyas
;; ;; el-autoyas
;; ;; clojure-snippets
;; ;; evil-magit
;; ;; smartparens
;; ;; helm
;; ;; window-numbering
;; ;; js2-mode
;; ;; ace-jump-mode
;; ;; ace-flyspell
;; ;; ace-jump-buffer
;; ;; multiple-cursors
;; ;; evil-avy
;; ;; tabbar
;; ;; elscreen
;; ;; goto-last-change
(require 'goto-last-change)
(global-set-key "\C-x\C-\\" 'goto-last-change)

;; ;; helm-swoop
;; ;; imenu-anywhere
;; ;; ace-window
;; ;; aggressive-indent
;; ;; flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-mudule-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; highlight-symbol
;; ;; whitespace
(require 'whitespace)
(global-set-key "\C-c_w" 'whitespace-mode)
(global-set-key "\C-c_t" 'whitespace-toggle-options)
(global-set-key "\C-c=w" 'global-whitespace-mode)
(global-set-key "\C-c=t" 'global-whitespace-toggle-options)
 ;; (defcustom whitespace-display-mappings
 ;;          '((space-mark   ?\    [?\xB7]     [?.])	; space
 ;;            (space-mark   ?\xA0 [?\xA4]     [?_])	; hard space
 ;;            (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])	; end-of-line
 ;;            ;; WARNING: the mapping below has a problem.
 ;;            ;; When a TAB occupies exactly one column, it will display the character
 ;;            ;; ?\xBB at that column followed by a TAB which goes to the next TAB
 ;;            ;; column.
 ;;            ;; If this is a problem for you, please, comment the line below.
 ;;            (tab-mark   ?\t   [?\xBB ?\t] [?\\ ?\t])	; tab
 ;;            ))

;; ;; linum-relative
;; ;; discover-my-major
(global-set-key (kbd "C-h M-m") 'discover-my-major)
(global-set-key (kbd "C-h M-S-m") 'discover-my-mode)
;; ;; fix-word
;; ;; multi-term
;; ;; auctex

;; ;; pdf-tools
;;;;  sudo apt-get install libpng-dev libz-dev libpoppler-glib-dev libpoppler-private-dev
;;;;  sudo apt-get install imagemagick

;; ;; evil-smartparens
(load "~/.emacs.d/setting/list-smartparens.el")


;; ;; evil-avy
(avy-setup-default)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-2)
;; (autoload 'evil-avy "evil-avy")
(define-key evil-normal-state-map "F" 'avy-goto-char2)
(define-key evil-normal-state-map "f" 'avy-goto-char)
(define-key evil-normal-state-map "e" 'avy-goto-word-1)
(define-key evil-normal-state-map "E" 'avy-goto-word-2)

;; ;; recentf configuration
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 30)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)




;;;; find-file-in-project-by-selected && projectile
(setq ffip-prefer-ido-mode t)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)


;; configuration
(key-chord-define-global "SS" (lambda()
		  (interactive)
		  (shell)))


;; ;; tramp
(require 'ssh)
(setq tramp-default-method "ssh")
(setq ad-redefinition-action 'accept)
(add-hook 'ssh-mode-hook 'ssh-directory-tracking-mode)
 (custom-set-variables '(ido-enable-tramp-completion nil))
(recentf-mode 1)


;; no tabs
(setq-default indent-tabs-mode nil)
(setq-default tabs-wdith 4)
(setq tramp-chunksize 500)
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

(global-set-key (kbd "<f12>")
		(lambda()
		  (interactive)
		  (find-file "~/.emacs.d/init.el")
		  (load-file "~/.emacs.d/init.el")))
(fset 'yes-or-no-p 'y-or-n-p)
(auto-image-file-mode t)
;; show match parenthesis
(show-paren-mode t)
;; display Tue Jun 24 01:07:07 2014
(display-time-mode 1)
;; mouse avoid
(mouse-avoidance-mode 'animate)
;; paste in middle 
(setq mouse-yank-at-point t)

;; allow outside paste
(setq x-select-enable-clipboard t)

(global-set-key (kbd "<f11>")
		(lambda()
		  (interactive)
		  (shell)))

(load "~/.emacs.d/setting/bookmark.el")
(setq python-shell-interpreter "./runpy")

