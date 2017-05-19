(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; カーソルの位置にデバッグ用のprintfを挿入 (C-c d)
(defun my-insert-printf-debug ()
  (interactive)
  (insert-string "printf(\"%s %s:%d\\n\", __func__, __FILE__, __LINE__);")
  (indent-according-to-mode))
(add-hook 'c-mode-hook
          (function (lambda ()
                      (define-key c-mode-map (kbd "C-c d") 'my-insert-printf-debug))))

(add-hook 'c++-mode-hook
          '(lambda()
             (setq indent-tabs-mode t)
             (setq indent-level 4)
             (setq c++-indent 4)
             (indent-tabs-mode nil)
             (setq tab-width 4)))
