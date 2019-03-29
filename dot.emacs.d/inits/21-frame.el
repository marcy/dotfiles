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

;; theme
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(load-theme 'doom-dracula t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

;(load-theme 'dracula t t)
;(enable-theme 'dracula)
