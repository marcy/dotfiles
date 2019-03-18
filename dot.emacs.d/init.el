(package-initialize)

(require 'cask)
(cask-initialize)

(require 'pallet)
(pallet-mode t)

;; インストールされたパッケージが package-selected-package 変数に保持されるのを抑制する
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;; init loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;;;;; after init
(add-hook 'after-init-hook
  (lambda ()
    ;; show init time
    (message "init time: %.3f sec"
             (float-time (time-subtract after-init-time before-init-time)))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

(load-theme 'dracula t t)
(enable-theme 'dracula)

;; EmacsにFocusが外れている際のFace
(defun my-out-focused-mode-line()
  (set-face-background 'mode-line "purple4"))
;; EmacsにFocusが当たっている際のFace
(defun my-in-focused-mode-line()
  (set-face-background 'mode-line "purple1"))

(add-hook 'focus-out-hook 'my-out-focused-mode-line)
(add-hook 'focus-in-hook 'my-in-focused-mode-line)

;;;;; which-key-mode
;(which-key-setup-side-window-bottom) ;ミニバッファ
(which-key-setup-side-window-right) ;右側
;(which-key-setup-side-window-right-bottom) ;両方使う

(which-key-mode 1)
