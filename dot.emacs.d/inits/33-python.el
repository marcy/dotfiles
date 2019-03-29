;; 保存時にバッファ全体を自動整形する
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(defun tnoda/turn-on-flycheck-mode ()
  (flycheck-mode 1))
(add-hook 'python-mode-hook 'tnoda/turn-on-flycheck-mode)
