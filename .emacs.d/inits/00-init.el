;;; lang
(setenv "LANG" "C")
(setenv "LC_TIME" "C")
(setenv "LC_MESSAGE" "C")
(setenv "LC_CTYPE" "C")
(setenv "LC_MONETARY" "C")
(set-language-environment "Japanese")

;;; utf8
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; my name, email
(setq user-full-name "Masashi Oyamada")
(setq user-mail-address "masashi.oyamada@gmail.com")

;; metaタグの内容で文字コードを自動判別しないように設定
(setq auto-coding-functions nil)

;;; server mode で起動
(load-library "server")(server-start)
(defalias 'exit 'save-buffers-kill-emacs)

;; ファイル保存時に行末の空行消す
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; package
(when (require 'package nil t)
  (package-initialize))

;; dash
(require 'dash)

;; elscreen
(elscreen-start)
(elscreen-set-prefix-key "\C-t")

;; exec-path-from-shell-initialize
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; scratch-ext
(require 'scratch-ext)

;; sequential-command-config
(require 'sequential-command-config)
(sequential-command-setup-keys)

(use-package auto-sudoedit
  :ensure t
  :config
  (auto-sudoedit-mode 1))
