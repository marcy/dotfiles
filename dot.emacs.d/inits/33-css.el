(setq cssm-indent-level 2)
(setq cssm-indent-function #'cssm-c-style-indenter) ;; インデントをまともに

(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(add-to-list 'auto-mode-alist '("\\.css$" . scss-mode))

;; インデント幅を2にする
;; コンパイルは compass watchで行うので自動コンパイルをオフ
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)
   )
  )
(add-hook 'scss-mode-hook
          '(lambda() (scss-custom)))
