;;; git
(global-git-gutter-mode t)
(add-hook 'ruby-mode-hook 'git-gutter-mode)

;;; magit
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-background 'magit-item-highlight "#000000") ; 選択項目ハイライトがうっとうしいので背景色と同化
;;      ;(set-face-background 'magit-item-highlight "#202020")
;;      (set-face-foreground 'magit-diff-add "#40ff40")
;;      (set-face-foreground 'magit-diff-del "#ff4040")
;;      (set-face-foreground 'magit-diff-file-header "#4040ff")))
