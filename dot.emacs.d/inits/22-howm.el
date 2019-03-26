(defvar howm-view-title-header "#")

(require 'howm)
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)

(setq howm-directory "~/Dropbox/howm/")
(setq howm-menu-recent-num 30)

(setq howm-file-name-format "%Y-%m-%d-%H%M%S.md")

;; 内容が 0 ならファイルごと消す
(if (not (memq 'delete-file-if-no-contests after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.md" (buffer-file-name (current-buffer)))
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
         (string-match "\\.md"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

(setq howm-keyword-file "~/Dropbox/tmp/dot.howm-keys")
(setq howm-history-file "~/Dropbox/tmp/dot.howm-history")
