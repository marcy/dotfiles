;; カレントバッファ全部 evernote へ
(defun toevernote ()
  (interactive)
  (shell-command (concat "~/Dropbox/bin/toevernote.rb " (buffer-file-name))))
