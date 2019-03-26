(require 'all-the-icons)
(require 'neotree)
;; 隠しファイルをデフォルトで表示
(setq neo-show-hidden-files t)
;; cotrol + q でneotreeを起動
(global-set-key "\C-q" 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
