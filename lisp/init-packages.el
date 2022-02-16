(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")

			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;;防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package keycast
  :ensure t
  :config
  (keycast-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'embark-act)
  (setq prefix-help-command 'embark-prefix-help-command))

(use-package consult
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "M-s i") 'consult-imenu))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/awesome-tab"))



(require 'awesome-tab)

(awesome-tab-mode t)

(defun awesome-tab-buffer-groups ()
"`awesome-tab-buffer-groups' control buffers' group rules.
Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
(list
(cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
	(memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode)))
    "Emacs")
    ((derived-mode-p 'eshell-mode)
    "EShell")
    ((derived-mode-p 'dired-mode)
    "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
    "OrgMode")
    ((derived-mode-p 'eaf-mode)
    "EAF")
    (t
     (awesome-tab-get-group-name (current-buffer))))))


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

 (define-key embark-file-map (kbd "E") #'consult-directory-externally)
;;打开当前文件的目录
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))

(use-package embark-consult
  :ensure t)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (eval-after-load
    'consult
  '(eval-after-load
       'embark
     '(progn
	(require 'embark-consult)
	(add-hook
	 'embark-collect-mode-hook
	 #'consult-preview-at-point-mode))))
  
  (defun embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
               (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                       (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
               (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                           (embark-export)))
      (x (user-error "embark category %S doesn't support writable export" x)))))
  
  (define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai 1))

(use-package magit
  :ensure t)

(package-install 'monokai-theme)

(load-theme 'monokai 1)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/awesome-tab"))

(require 'awesome-tab)

(awesome-tab-mode t)

(provide 'init-packages)
