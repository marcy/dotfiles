(setq whitespace-style '(face                   ; faceを使って視覚化する。
                         tabs                   ; 行末の空白を対象とする。
                         tab-mark
                         spaces
                         space-mark
                         ;lines-tail            ; 長すぎる行のうち
                                                ; whitespace-line-column以降のみを
                                                ; 対象とする。
                         trailing
                         space-before-tab       ; タブの前にあるスペースを対象とする。
                         space-after-tab::space
                         ;space-after-tab       ; タブの後にあるスペースを対象とする。
                         ))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(global-whitespace-mode t)

(set-face-attribute 'whitespace-trailing nil
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :foreground "labelColor"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :foreground "GreenYellow"
                    :weight 'bold)
