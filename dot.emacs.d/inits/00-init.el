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

;; elscreen
(elscreen-start)
(elscreen-set-prefix-key "\C-t")

;; exec-path-from-shell-initialize
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(unless (eq window-system nil)
  (progn
    (menu-bar-mode nil)        ; メニューバーなし
    (tool-bar-mode -1)         ; ツールバーなし
    (set-scroll-bar-mode nil)  ; スクロールバーなし
    ))

;; バッファの終わりをフリンジに表示
(setq-default indicate-buffer-boundaries
              '((top . nil) (bottom . right) (down . right)))

;; Cocoa Emacsのフォントセットを定義
(when (eq window-system 'ns)
  (create-fontset-from-ascii-font
   "Menlo-12:weight=normal:slant=normal"
   nil
   "menlokakugo")
  (set-fontset-font
   "fontset-menlokakugo"
   'unicode
   (font-spec :family "Hiragino Kaku Gothic ProN" :size 12)
   nil
   'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
  (add-to-list 'initial-frame-alist '(font . "fontset-menlokakugo"))
  (add-to-list 'face-font-rescale-alist
               '(".*Hiragino Kaku Gothic ProN.*" . 1.2))
  (add-hook 'after-init-hook
            (lambda () (set-frame-font "fontset-menlokakugo"))))

;; frame
(when (eq window-system 'ns)
  (set-frame-parameter nil 'alpha '(95 75))) ; 透明に

;; EmacsにFocusが外れている際のFace
(defun my-out-focused-mode-line()
  (set-face-background 'mode-line "purple4"))
;; EmacsにFocusが当たっている際のFace
(defun my-in-focused-mode-line()
  (set-face-background 'mode-line "purple1"))

(add-hook 'focus-out-hook 'my-out-focused-mode-line)
(add-hook 'focus-in-hook 'my-in-focused-mode-line)

;; scratch-ext
(require 'scratch-ext)

;; sequential-command-config
(require 'sequential-command-config)
(sequential-command-setup-keys)
