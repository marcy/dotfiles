;;; init.el --- Emacs Configuration
;;; Commentary:
;; この設定ファイルは `leaf` を用いてパッケージ管理を行っています。
;;; Code:

;; ------------------------------------------------------------
;; Setup: ディレクトリ設定とパッケージ管理
;; ------------------------------------------------------------
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  ;; パッケージアーカイブの設定
  (customize-set-variable
   'package-archives '(("melpa"  . "https://melpa.org/packages/")
                       ("gnu"    . "https://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  ;; `leaf` がなければインストール
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  ;; leaf-keywords の初期化（必要なオプションパッケージも読み込み）
  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra    :ensure t)
    (leaf el-get   :ensure t)
    (leaf blackout :ensure t)
    :config (leaf-keywords-init)))

;; ------------------------------------------------------------
;; 基本設定 (Built-in customization)
;; ------------------------------------------------------------
(leaf cus-edit
  :doc "Emacs と Lisp パッケージのカスタマイズ用ツール"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "組み込み機能の基本設定"
  :tag "builtin" "internal"
  :custom '((user-full-name           . "Masashi Oyamada")
            (user-mail-address        . "masashi.oyamada@gmail.com")
            (history-length           . t)
            (history-delete-duplicates . t)
            (truncate-lines           . nil)
            (menu-bar-mode            . nil)
            (tool-bar-mode            . nil)
            (scroll-bar-mode          . nil)
            (indent-tabs-mode         . nil)
            (use-short-answers        . t))
  :config
  (defalias 'exit 'save-buffers-kill-emacs))

(leaf server
  :doc "Emacs をサーバーモードで起動"
  :tag "builtin"
  :added "2020-09-02"
  :config (server-start))

;; ------------------------------------------------------------
;; ユーティリティ・環境設定
;; ------------------------------------------------------------
;; 保存時に末尾空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq default-directory "~/")

(leaf-keys (("C-c h" . help-for-help)
            ("C-x C-c" . server-edit)
            ("C-h" . delete-backward-char)
            ("C-@" . mark-word)
            ("C-x l" . goto-line)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :config
    (setq imenu-list-size 30)
    (setq imenu-list-position 'left)))

;; ------------------------------------------------------------
;; 組み込みのマイナーモード設定
;; ------------------------------------------------------------
(leaf autorevert
  :doc "ファイル変更時にバッファを自動更新"
  :tag "builtin"
  :added "2025-02-19"
  :config (global-auto-revert-mode 1))

(leaf delsel
  :doc "選択状態にあるときは文字入力で置換"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "対応する括弧をハイライト表示"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

;; ------------------------------------------------------------
;; タブバーの設定
;; ------------------------------------------------------------
(leaf tab-bar
  :doc "フレームローカルのタブ機能"
  :tag "builtin"
  :added "2024-12-09"
  :config
  (global-unset-key (kbd "C-t"))
  (define-prefix-command 'my-tab-bar-map)
  (global-set-key (kbd "C-t") 'my-tab-bar-map)
  ;; 基本操作のキーバインド
  (dolist (key-func '(("c" . tab-new)
                      ("k" . tab-close)
                      ("n" . tab-next)
                      ("p" . tab-previous)))
    (define-key my-tab-bar-map (kbd (car key-func)) (cdr key-func)))
  ;; 数字キーによるタブ選択
  (dotimes (i 9)
    (define-key my-tab-bar-map (kbd (number-to-string (1+ i)))
                `(lambda () (interactive) (tab-bar-select-tab ,(1+ i)))))
  (custom-set-faces
   '(tab-bar ((t (:background "gray10"))))
   '(tab-bar-tab ((t (:background "gray75" :foreground "black" :box nil))))
   '(tab-bar-tab-inactive ((t (:background "gray30" :foreground "gray80" :box nil))))))

(leaf exec-path-from-shell
  :doc "シェルから環境変数 ($PATH 等) を取得"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-28"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)))

(leaf scratch-ext
  :doc " *scratch* バッファ拡張"
  :added "2020-08-28"
  :url "https://github.com/kyanagi/scratch-ext-el"
  :ensure t
  :setq ((scratch-ext-log-directory   . "~/Dropbox/junk/")
         (scratch-ext-log-name-format . "%Y/%m-%d-%H%M%S.scratch.txt"))
  :require t)

;; ------------------------------------------------------------
;; フォント・テーマ設定
;; ------------------------------------------------------------
(leaf font
  :when (memq window-system '(mac ns))
  :config
  (let* ((size 16)
         (asciifont "UDEV Gothic NF")
         (jpfont "UDEV Gothic NF")
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font nil '(#x0080 . #x024F) fontspec)
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)))

(leaf doom-themes
  :doc "最新のカラー・テーマパック"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
  :added "2020-08-28"
  :url "https://github.com/hlissner/emacs-doom-theme"
  :emacs>= 25.1
  :ensure t
  :custom ((doom-themes-enable-bold   . t)
           (doom-themes-enable-italic . t))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf whitespace
  :doc "空白文字の可視化"
  :tag "builtin"
  :config
  ;; リテラルではなく、動的にリストを生成して変数に設定する
  (setq whitespace-style (list 'face 'tabs 'tab-mark 'spaces 'space-mark 'trailing 'space-before-tab 'space-after-tab))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
        (list (list 'space-mark 12288 [9633])
              (list 'tab-mark 9 [187 9])))
  (global-whitespace-mode t)
  (set-face-attribute 'whitespace-trailing nil :foreground "DeepPink" :underline t)
  (set-face-attribute 'whitespace-tab nil :foreground "grey20" :background 'unspecified :underline t)
  (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :weight 'bold))

;; ------------------------------------------------------------
;; Helm, Corfu などの補完・ナビゲーションツール
;; ------------------------------------------------------------
(leaf helm
  :doc "インクリメンタル・フレームワーク"
  :req "emacs-25.1" "async-1.9.4" "popup-0.5.3" "helm-core-3.6.2"
  :tag "emacs>=25.1"
  :added "2020-08-28"
  :url "https://emacs-helm.github.io/helm/"
  :emacs>= 25.1
  :ensure t
  :init
  ;; helm-ghq
  (leaf helm-ghq
    :doc "Ghq を Helm インタフェースで利用"
    :req "emacs-24" "helm-2.2.0"
    :tag "emacs>=24"
    :added "2020-08-28"
    :url "https://github.com/masutaka/emacs-helm-ghq"
    :emacs>= 24
    :ensure t
    :bind (("C-x p" . helm-ghq)))
  ;; helm-projectile
  (leaf helm-projectile
    :doc "Projectile との統合"
    :req "helm-1.9.9" "projectile-2.2.0" "cl-lib-0.3"
    :tag "convenience" "project"
    :added "2020-08-31"
    :url "https://github.com/bbatsov/helm-projectile"
    :ensure t
    :bind (("C-:" . helm-projectile))
    :config (helm-projectile-on))
  ;; helm-bundle-show
  (leaf helm-bundle-show
    :doc "Helm で Bundle を表示"
    :req "emacs-24" "helm-1.8.0"
    :tag "emacs>=24"
    :added "2020-08-31"
    :url "https://github.com/masutaka/emacs-helm-bundle-show"
    :emacs>= 24
    :ensure t
    :bind (("C-x y" . helm-bundle-show))
    :after helm)
  :bind (("C-;" . helm-mini)
         ("C-x b" . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         (helm-map ("C-h" . delete-backward-char)))
  :config
  (helm-mode t)
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
  (setq helm-delete-minibuffer-contents-from-point t)
  (advice-add 'helm-delete-minibuffer-contents :before
              (lambda (&rest _)
                "Kill line in helm minibuffer."
                (kill-new (buffer-substring (point) (field-end)))))
  (advice-add 'helm-ff-kill-or-find-buffer-fname :around
              (lambda (orig-fn &rest args)
                "実在する候補のみ処理を実行。"
                (when (and (boundp 'candidate) (file-exists-p candidate))
                  (apply orig-fn args))))
  (advice-add 'helm-ff-transform-fname-for-completion :around
              (lambda (_orig-fn pattern &rest _args)
                "パターンを変換して意図した補完を実現。"
                (let* ((input-pattern (file-name-nondirectory pattern))
                       (dirname (file-name-directory pattern)))
                  (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
                  (concat dirname
                          (if (string-match "^\\^" input-pattern)
                              (substring input-pattern 1)
                            (concat ".*" input-pattern)))))))

(leaf corfu
  :doc "ミニマルな補完 UI"
  :ensure t
  :custom ((corfu-auto        . t)
           (corfu-auto-delay  . 0)
           (corfu-auto-prefix . 2)
           (corfu-cycle       . t))
  :bind ((corfu-map ("C-n" . corfu-next))
         (corfu-map ("C-p" . corfu-previous))
         (corfu-map ("C-s" . corfu-insert-separator))
         (corfu-map ("C-i" . corfu-complete)))
  :global-minor-mode global-corfu-mode
  :init
  (leaf cape
    :doc "Completion At Point Extensions"
    :ensure t
    :config
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(leaf completion-preview
  :doc "入力中に補完候補をインラインでプレビュー"
  :tag "builtin"
  :global-minor-mode global-completion-preview-mode)

;; ------------------------------------------------------------
;; セッション管理と最近使ったファイル
;; ------------------------------------------------------------
(leaf savehist
  :doc "ミニバッファの履歴をセッション間で保持"
  :tag "builtin"
  :custom ((savehist-additional-variables
            . '(kill-ring search-ring regexp-search-ring)))
  :global-minor-mode savehist-mode)

(leaf saveplace
  :doc "前回のカーソル位置を記憶・復元"
  :tag "builtin"
  :global-minor-mode save-place-mode)

(leaf recentf
  :doc "最近開いたファイルのメニュー"
  :tag "builtin"
  :added "2020-08-31"
  :setq ((recentf-max-menu-items . 200)
         (recentf-max-saved-items . 3000))
  :config (recentf-mode t))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; ------------------------------------------------------------
;; バージョン管理 (Git)
;; ------------------------------------------------------------
(leaf magit
  :doc "Emacs 内で Git を操作"
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516"
  "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2020-08-28"
  :emacs>= 25.1
  :ensure t
  :after git-commit with-editor)

;; ------------------------------------------------------------
;; 言語別モード設定 (Markdown, Ruby, など)
;; ------------------------------------------------------------
;; Markdown
(leaf markdown-mode
  :doc "Markdown 用のメジャーモード"
  :req "emacs-25.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=25.1"
  :added "2020-09-23"
  :url "https://jblevins.org/projects/markdown-mode/"
  :emacs>= 25.1
  :ensure t
  :setq ((markdown-asymmetric-header . t)
         (markdown-header-scaling   . t))
  :config
  (setq auto-mode-alist
        (append auto-mode-alist
                '(("\\.md\\'" . markdown-mode)
                  ("\\.markdown\\'" . markdown-mode)))))

;; Ruby 関連
(leaf ruby-mode
  :doc "Ruby ファイル編集用のメジャーモード"
  :tag "builtin"
  :added "2020-08-28"
  :mode ("\\.rb\\'" "Gemfile" "Rakefile"))

(leaf rspec-mode
  :doc "RSpec 用の拡張 (Ruby)"
  :req "ruby-mode-1.0" "cl-lib-0.4"
  :tag "ruby" "rspec"
  :added "2020-10-07"
  :url "http://github.com/pezra/rspec-mode"
  :ensure t
  :after ruby-mode)

(leaf haml-mode
  :doc "Haml ファイル用のメジャーモード"
  :req "emacs-24" "cl-lib-0.5"
  :tag "html" "languages" "markup" "emacs>=24"
  :added "2021-01-14"
  :url "https://github.com/nex3/haml-mode"
  :emacs>= 24
  :ensure t)

(leaf flycheck
  :doc "リアルタイム構文チェック"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :added "2020-08-28"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :config
  (flycheck-define-checker ruby-rubocop
    "Ruby の構文・スタイルチェッカー (RuboCop 使用)
See URL `http://batsov.com/rubocop/'."
    :command ("rubocop" "--display-cop-names" "--format" "emacs"
              (config-file "--config" flycheck-rubocoprc)
              (option-flag "--lint" flycheck-rubocop-lint-only)
              "--stdin" source-original)
    :standard-input t
    :error-patterns
    ((info line-start (file-name) ":" line ":" column ": C: "
           (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": W: "
              (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
     (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
            (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end))
    :modes (enh-ruby-mode ruby-mode)))

;; ------------------------------------------------------------
;; その他のツール
;; ------------------------------------------------------------
(leaf open-junk-file
  :doc "試行錯誤用の Junk ファイルを開く"
  :tag "tools" "convenience"
  :added "2020-08-28"
  :url "http://www.emacswiki.org/cgi-bin/wiki/download/open-junk-file.el"
  :bind (("C-x j" . open-junk-file))
  :ensure t
  :setq ((open-junk-file-directory . "~/Dropbox/junk/%Y/%m-%d-%H%M%S.")))

(leaf ag
  :doc "ag ('the silver searcher') のフロントエンド"
  :req "dash-2.8.0" "s-1.9.0" "cl-lib-0.5"
  :added "2020-08-28"
  :ensure t
  :bind (("C-x C-g" . ag))
  :config
  (leaf wgrep-ag
    :doc "ag の検索結果を編集してファイルに反映"
    :req "wgrep-2.3.2"
    :tag "extensions" "edit" "grep"
    :added "2020-08-28"
    :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el"
    :ensure t
    :after wgrep
    :config
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)
    (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)))

(leaf wdired
  :doc "dired バッファ内でファイル名を編集してリネーム"
  :tag "builtin"
  :added "2020-08-28"
  :config (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

(leaf nerd-icons
  :doc "Nerd Fonts アイコンライブラリ (all-the-icons の後継)"
  :ensure t
  :init
  (leaf nerd-icons-dired
    :doc "dired でファイル毎にアイコン表示"
    :ensure t
    :config (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)))

(leaf yaml-mode
  :doc "YAML ファイル用メジャーモード"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :added "2020-08-31"
  :emacs>= 24.1
  :ensure t)

;; ------------------------------------------------------------
;; Provide
;; ------------------------------------------------------------
(provide 'init)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;; init.el ends here
