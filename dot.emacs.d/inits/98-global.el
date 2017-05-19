(progn
  (bind-key "\C-ch" 'help-for-help)
  (bind-key "C-x C-c" 'server-edit)
  (bind-key* "\C-h" 'delete-backward-char)
 )

;;; keybind
(define-key global-map "\C-c;" 'comment-region)      ; コメントアウト
(define-key global-map "\C-c:" 'uncomment-region)    ; コメント解除
(define-key global-map "\C-c\C-u" 'universal-argument)
(define-key global-map "\C-@" 'mark-word)

;; ファイル保存時に行末の空行消す
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; goto-line
(define-key global-map "\C-xl" 'goto-line)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)
