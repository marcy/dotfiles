;; dired バッファを編集して多数のファイルを一括リネームできる．
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

; シマシマにする
; (add-hook 'dired-mode-hook 'stripe-listify-buffer)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)