(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
       '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
       '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


(defvar myPackages
  '(ess
    ssh
    recentf
    projectile
    helm-projectile
    elpy
    ein
    better-defaults
    material-theme
    maxframe
    smex
    auto-complete
    org
    org-plus-contrib
    evil
    key-chord
    rainbow-delimiters
    jinja2-mode
    yasnippet
    r-autoyas
    el-autoyas
    clojure-snippets
    evil-magit
    smartparens
    helm
    window-numbering
    js2-mode
    ace-jump-mode
    ace-flyspell
    ace-jump-buffer
    multiple-cursors
    avy
    evil-avy
    tabbar
    elscreen
    goto-last-change
    helm-swoop
    imenu-anywhere
    ace-window
    aggressive-indent
    flycheck
    highlight-symbol
    whitespace
    linum-relative
    discover-my-major
    fix-word
    multi-term
    auctex
    pdf-tools
    evil-smartparens
    find-file-in-project
    desktop))

(mapc #' (lambda (package)
	   (unless (package-installed-p package)
	     (package-install package)))
	 myPackages)
(provide 'install-packages)
