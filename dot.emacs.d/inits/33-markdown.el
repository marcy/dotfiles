(setq markdown-asymmetric-header t)
;(setq markdown-marginalize-headers t)
(setq markdown-header-scaling t)

(setq auto-mode-alist (append
                       auto-mode-alist
                       (list
                        '("\\.md" . markdown-mode)
                        '("\\.markdown" . markdown-mode)
                        )))

(if (executable-find "pandoc")
    (setq markdown-command "pandoc -s --self-contained -t html5 -c ~/.pandoc/github-markdown.css"))
