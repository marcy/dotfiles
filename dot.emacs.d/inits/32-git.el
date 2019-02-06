;;; git
(global-git-gutter-mode t)
(add-hook 'ruby-mode-hook 'git-gutter-mode)

;;; magit
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; vc-annotateで現在の行がmergeされたPRを開く
(require 'vc-annotate)

(defun vc-annotate-open-pr-at-line ()
  (interactive)
  (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
         (rev (car rev-at-line)))
    (shell-command (concat "open-pr-from-commit " rev))))

(define-key vc-annotate-mode-map (kbd "P") 'vc-annotate-open-pr-at-line)
