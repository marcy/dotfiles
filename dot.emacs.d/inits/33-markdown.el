(setq auto-mode-alist (append
                       auto-mode-alist
                       (list
                        '("\\.md" . markdown-mode)
                        '("\\.markdown" . markdown-mode)
                        )))

(custom-set-faces
 '(markdown-header-delimiter-face ((t (:inherit org-mode-line-clock))))
 '(markdown-header-face-1 ((t (:inherit outline-1 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :weight bold))))
 '(markdown-pre-face ((t (:inherit org-formula))))
 )

(if (executable-find "pandoc")
    (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.pandoc/github-markdown.css"))
