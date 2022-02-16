(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-packages)
(require 'init-keybinding)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)
