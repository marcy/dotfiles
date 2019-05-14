(unless (eq window-system nil)
  (progn
    (menu-bar-mode nil)        ; メニューバーなし
    (tool-bar-mode -1)         ; ツールバーなし
    (set-scroll-bar-mode nil)  ; スクロールバーなし
    ))

;; バッファの終わりをフリンジに表示
(setq-default indicate-buffer-boundaries
              '((top . nil) (bottom . right) (down . right)))

;; font
(when (eq window-system 'ns)
  (create-fontset-from-ascii-font
   "Ricty Diminished-14:weight=normal:slant=normal"
   nil
   "ricty_diminished")
  (set-fontset-font
   "fontset-ricty_diminished"
   'unicode
   (font-spec :family "Ricty Diminished" :weight 'normal :slant 'normal)
   nil
   'append)
  (add-to-list 'default-frame-alist
               '(font . "fontset-ricty_diminished"))
  (add-to-list 'initial-frame-alist
               '(font . "fontset-ricty_diminished")))

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
