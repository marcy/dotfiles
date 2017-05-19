;; dired バッファを編集して多数のファイルを一括リネームできる．
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

; シマシマにする
; (add-hook 'dired-mode-hook 'stripe-listify-buffer)
