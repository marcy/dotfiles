(column-number-mode 1)          ; カーソルの位置が何文字目かを表示する
(line-number-mode 1)            ; カーソルの位置が何行目かを表示する

(unless (eq window-system nil)
  (progn
    (menu-bar-mode nil)        ; メニューバーなし
    (tool-bar-mode -1)         ; ツールバーなし
    (set-scroll-bar-mode nil)  ; スクロールバーなし
    ))

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

;;; タブではなくスペースを使う
(setq-default indent-tabs-mode nil)

;;; ============================================================
;;;    Cocoa Emacsのフォントセットを定義
;;; ============================================================
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

;;; ============================================================
;;;             frame
;;; ============================================================
(when (eq window-system 'ns)
  ;; 透明に
  (set-frame-parameter nil 'alpha '(90 75)))

;; color-theme-modern
(load-theme 'dark-laptop t t)
(enable-theme 'dark-laptop)
