;; key bindings
(bind-key "C-c h" 'help-for-help)
(bind-key "C-x C-c" 'server-edit)
(bind-key "C-h" 'delete-backward-char)
(bind-key "C-c ;" 'comment-region)      ; コメントアウト
(bind-key "C-c :" 'uncomment-region)    ; コメント解除
(bind-key "C-c C-u" 'universal-argument)
(bind-key "C-@" 'mark-word)
(bind-key "C-x l" 'goto-line)
(bind-key "C-x j" 'open-junk-file)
(bind-key "C-q" 'neotree-toggle)
(bind-key "C-M-i" 'company-complete)
;(bind-key "" 'highlight-symbol-at-point)
(bind-key "C-c C-r" 'window-resizer)
(bind-key "C-c , ," 'howm-menu)
(bind-key "C-x g" 'ag)
(bind-key "C-;" 'helm-mini)
(bind-key "C-:" 'helm-projectile)
(bind-key "M-x" 'helm-M-x)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x C-r" 'helm-recentf)
(bind-key "M-y" 'helm-show-kill-ring)
(bind-key "C-c i" 'helm-imenu)
(bind-key "C-x b" 'helm-buffers-list)
(bind-key "C-x f" 'helm-ls-git-ls)
(bind-key "C-x p" 'helm-ghq)
(bind-key "C-x y" 'helm-bundle-show)
;(bind-key "RET" 'smart-newline)
