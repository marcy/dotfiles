(add-hook 'ruby-mode-hook
  (lambda ()
    (smart-newline-mode t)))

(define-key global-map (kbd "RET") 'smart-newline)
