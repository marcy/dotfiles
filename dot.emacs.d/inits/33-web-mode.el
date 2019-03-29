;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.eex$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"   . web-mode))

;;; インデント数
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
