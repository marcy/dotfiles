(require 'py-autopep8)

;; 保存時にバッファ全体を自動整形する
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
