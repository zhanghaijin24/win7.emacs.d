(require 'package)
(setq package-archives '(("gnu" . "http://elpa.zilongshanren.com/gnu/")
			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

(global-set-key (kbd "M-1") 'bookmark-jump)

(set-face-attribute 'default nil :height 160)

(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(global-linum-mode 1)

(setq inhibit-startup-screen t)

(toggle-frame-maximized)

(setq make-backup-files nil)

(global-auto-revert-mode t)

(setq ring-bell-function 'ignore)


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'cdlatex)
(mapc (lambda (mode)
	(add-hook 'LaTeX-mode-hook mode))
      (list 'turn-on-cdlatex
	    'reftex-mode
	    'outline-minor-mode
	    'auto-fill-mode
	    'flyspell-mode
	    'hide-body t))

(setq outline-minor-mode-prefix [(control o)])

(setq preview-gs-command "\"c:/texlive/2022/bin/win32/rungs.exe\"")
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)

(setq TeX-view-program-list
      '(("SumatraPDF"
	 ("\"c:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
	  (mode-io-correlate " -forward-search %b %n ") " %o"))))
(setq TeX-view-program-selection '((output-pdf "SumatraPDF")))

(defun consult-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
	   (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
	    (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
		    ('darwin "open")
		    ('cygwin "cygstart")
		    (_ "xdg-open"))
		  nil 0 nil
		  (file-name-directory (expand-file-name file)))))

 ;;(define-key embark-file-map (kbd "E") #'consult-directory-externally)
(global-set-key (kbd "<f3>") 'my-open-current-directory)
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))

(server-mode 1)

(progn
	(defvar emax-root (concat (expand-file-name "~") "/emax"))
	(defvar emax-bin (concat emax-root "/bin"))
	(defvar emax-bin64 (concat emax-root "/bin64"))

	(setq exec-path (cons emax-bin exec-path))
	(setenv "PATH" (concat emax-bin ";" (getenv "PATH")))

	(setq exec-path (cons emax-bin64 exec-path))
	(setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

	(setq emacsd-bin (concat user-emacs-directory "bin"))
	(setq exec-path (cons  emacsd-bin exec-path))
	(setenv "PATH" (concat emacsd-bin  ";" (getenv "PATH")))

	;;可选安装msys64
	;;下载地址: http://repo.msys2.org/mingw/sources/
	(setenv "PATH" (concat "C:\\msys64\\usr\\bin;C:\\msys64\\mingw64\\bin;" (getenv "PATH")))

	;; (dolist (dir '("~/emax/" "~/emax/bin/" "~/emax/bin64/" "~/emax/lisp/" "~/emax/elpa/"))
	;;   (add-to-list 'load-path dir))
	)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-files (list "~/quickstart/content/posts/orgfiles/gcal.org"
                             "~/quickstart/content/posts/orgfiles/novel.org"
			     "~/quickstart/content/posts/orgfiles/github.org"
                            "~/quickstart/content/posts/orgfiles/schedule.org")) 

(setq org-capture-templates
      '(("a" "Appointment" entry (file+headline "~/quickstart/content/posts/orgfiles/gcal.org" "Appointments")
                       "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
		      
        ("g" "github" entry (file+headline "~/quickstart/content/posts/orgfiles/github.org" "linux")
         "* %?\n%T" :prepend t)
			      
        ("l" "link" entry (file+headline "~/quickstart/content/posts/orgfiles/links.org" "Links")
         "* %? %^L %^g \n%T" :prepend t)
			     
        ("h" "haijin" entry (file+headline "~/quickstart/content/posts/orgfiles/novel.org" "haijin")
         "* %?\n%T" :prepend t)
			     
        ("n" "novel" entry (file+headline "~/quickstart/content/posts/orgfiles/novel.org" "author")
         "* %?\n%T" :prepend t)

        ("j" "Journal" entry (file+datetree "~/quickstart/content/posts/orgfiles/journal.org")
         "* %?\nEntered on %U\n %i\n %a")
			     
        ("s" "Screencast" entry (file "~/quickstart/content/posts/orgfiles/screencastnotes.org")
         "* %?\n%i\n")))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai 1))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (setq inhibit-compacting-font-caches t))

(use-package magit
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit org-bullets org-bullet company auctex use-package monokai-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
