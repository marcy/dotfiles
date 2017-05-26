(require 'cask)
(cask-initialize)

(require 'pallet)
(pallet-mode t)

;; インストールされたパッケージが package-selected-package 変数に保持されるのを抑制する
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; init loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;; after init
(add-hook 'after-init-hook
  (lambda ()
    ;; show init time
    (message "init time: %.3f sec"
             (float-time (time-subtract after-init-time before-init-time)))))
