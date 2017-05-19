(require 'howm)
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)

(add-to-list 'auto-mode-alist '("\\.howm$" . markdown-mode))

(setq howm-directory "~/Dropbox/dotfiles/emacs/howm/")
(setq howm-menu-recent-num 50)

;; 済み(.)は表示しないように
(setq howm-todo-menu-types "[-+~!]")
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")

;; 内容が 0 ならファイルごと消す
(if (not (memq 'delete-file-if-no-contests after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)

;; セーブしてバッファも kill する
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

;; 保存時処理
;; 保存時にパーミッションを"600"にする。
;; (add-hook'howm-after-save-hook
;;  (lambda ()
;;    (let ((name (buffer-file-name)))
;;         (set-file-modes name 384)))) ; 600

(setq howm-keyword-file "~/Dropbox/dotfiles/emacs/dot.howm-keys")
(setq howm-history-file "~/Dropbox/dotfiles/emacs/dot.howm-history")
