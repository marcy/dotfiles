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

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

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

(column-number-mode 1)          ; カーソルの位置が何文字目かを表示する
(line-number-mode 1)            ; カーソルの位置が何行目かを表示する

(unless (eq window-system nil)
  (progn
    (menu-bar-mode nil)        ; メニューバーなし
    (tool-bar-mode -1)         ; ツールバーなし
    (set-scroll-bar-mode nil)  ; スクロールバーなし
    ))

;; バッファの終わりをフリンジに表示
(setq-default indicate-buffer-boundaries
              '((top . nil) (bottom . right) (down . right)))

;; color
(show-paren-mode t)                     ; 括弧に色
(setq show-paren-style 'parenthesis)
(global-font-lock-mode t)               ; 字に色

;;; リージョンの色
(transient-mark-mode t)

;;; 右端で折り返さない
(setq truncate-lines nil)

;; 分割したウィンドウでも右端で折り返さない
(setq truncate-partial-width-windows nil)

;; タブではなくスペースを使う
(setq-default indent-tabs-mode nil)

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
(setq scratch-ext-log-directory "~/Dropbox/scratch/")

;; junk-file
(setq open-junk-file-format "~/Dropbox/junk/%Y/%m-%d-%H%M%S.")

;; sequential-command-config
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; keyfreq-mode
(setq keyfreq-file "~/Dropbox/dotfiles/local/emacs.keyfreq")
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
