(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "Masashi Oyamada")
            (user-mail-address . "masashi.oyamada@gmail.com")
            (history-length . t)
            (history-delete-duplicates . t)
            (truncate-lines . nil)
            (menu-bar-mode . nil)
            (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

;;; lang
(setenv "LANG" "C")
(setenv "LC_TIME" "C")
(setenv "LC_MESSAGE" "C")
(setenv "LC_CTYPE" "C")
(setenv "LC_MONETARY" "C")
(set-language-environment "Japanese")

;;; utf8
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; metaタグの内容で文字コードを自動判別しないように設定
(setq auto-coding-functions nil)

;;; server mode で起動
(load-library "server")
(server-start)
(defalias 'exit 'save-buffers-kill-emacs)

;; ファイル保存時に行末の空行消す
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(leaf dash
  :doc "A modern list library for Emacs"
  :tag "lists"
  :added "2020-08-28"
  :ensure t)

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf elscreen
  :doc "Emacs window session manager"
  :req "emacs-24"
  :tag "convenience" "window" "emacs>=24"
  :added "2020-08-28"
  :url "https://github.com/knu/elscreen"
  :emacs>= 24
  :ensure t
  :custom-face ((elscreen-tab-background-face quote
                                              ((t
                                                (:background "gray10"))))
                (elscreen-tab-control-face quote
                                           ((t
                                             (:background "gray10" :foreground "gray60"))))
                (elscreen-tab-current-screen-face quote
                                                  ((t
                                                    (:background "gray75" :foreground "black"))))
                (elscreen-tab-other-screen-face quote
                                                ((t
                                                  (:background "gray30" :foreground "gray80")))))
  :config
  (elscreen-start)
  (elscreen-set-prefix-key "\C-t"))

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2020-08-28"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(leaf scratch-ext
  :doc "Extensions for *scratch*"
  :added "2020-08-28"
  :url "https://github.com/kyanagi/scratch-ext-el"
  :ensure t
  :setq
  ((scratch-ext-log-directory . "~/Dropbox/junk/")
   (scratch-ext-log-name-format . "%Y/%m-%d-%H%M%S.scratch.txt"))
  :require t)

(leaf sequential-command
  :doc "Many commands into one command"
  :tag "lisp" "convenience"
  :added "2020-08-28"
  :url "http://www.emacswiki.org/cgi-bin/wiki/download/sequential-command.el"
  :ensure t)

(leaf sequential-command-config
  :doc "Examples of sequential-command.el"
  :tag "out-of-MELPA" "convenience" "extensions"
  :added "2020-08-28"
  :url "http://www.emacswiki.org/cgi-bin/wiki/download/sequential-command-config.el"
  :require t
  :config
  (sequential-command-setup-keys))

(leaf auto-sudoedit
  :doc "Auto sudo edit by tramp"
  :req "emacs-24.4" "f-0.19.0"
  :tag "emacs>=24.4"
  :added "2020-08-28"
  :url "https://github.com/ncaq/auto-sudoedit"
  :emacs>= 24.4
  :ensure t)

(leaf frame
  :unless (eq window-system nil)
  :setq-default ((indicate-buffer-boundaries quote
                                             ((top)
                                              (bottom . right)
                                              (down . right)))))

(leaf font
  :when (eq window-system 'ns)
  :config
  (create-fontset-from-ascii-font "Ricty Diminished-14:weight=normal:slant=normal" nil "ricty_diminished")
  (set-fontset-font "fontset-ricty_diminished" 'unicode
                    (font-spec :family "Ricty Diminished" :weight 'normal :slant 'normal)
                    nil 'append)
  (add-to-list 'default-frame-alist
               '(font . "fontset-ricty_diminished"))
  (add-to-list 'initial-frame-alist
               '(font . "fontset-ricty_diminished"))
  (set-frame-parameter nil 'alpha
                       '(95 75)))

;; theme
(leaf doom-themes
  :doc "an opinionated pack of modern color-themes"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
  :added "2020-08-28"
  :url "https://github.com/hlissner/emacs-doom-theme"
  :emacs>= 25.1
  :ensure t
  :custom ((doom-themes-enable-bold . t)
           (doom-themes-enable-italic . t))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf color-moccur
  :doc "multi-buffer occur (grep) mode"
  :tag "convenience"
  :added "2020-08-28"
  :url "http://www.bookshelf.jp/elc/color-moccur.el"
  :ensure t
  :custom ((moccur-use-migemo . nil))
  :config
  (defun toggle-moccur-use-migemo ()
    (interactive)
    (setq moccur-use-migemo (if moccur-use-migemo
                                nil
                              t))
    (message "%s" moccur-use-migemo)))

(leaf helm
  :doc "Helm is an Emacs incremental and narrowing framework"
  :req "emacs-25.1" "async-1.9.4" "popup-0.5.3" "helm-core-3.6.2"
  :tag "emacs>=25.1"
  :added "2020-08-28"
  :url "https://emacs-helm.github.io/helm/"
  :emacs>= 25.1
  :require t
  :ensure t
  :init
  (leaf helm-config
    :doc "Applications library for `helm.el'"
    :tag "out-of-MELPA"
    :added "2020-08-31"
    :require t)
  (leaf helm-ghq
    :doc "Ghq with helm interface"
    :req "emacs-24" "helm-2.2.0"
    :tag "emacs>=24"
    :added "2020-08-28"
    :url "https://github.com/masutaka/emacs-helm-ghq"
    :emacs>= 24
    :ensure t
    :bind
    (("C-x p" . helm-ghq)))
  (leaf helm-projectile
    :doc "Helm integration for Projectile"
    :req "helm-1.9.9" "projectile-2.2.0" "cl-lib-0.3"
    :tag "convenience" "project"
    :added "2020-08-31"
    :url "https://github.com/bbatsov/helm-projectile"
    :ensure t
    :bind (("C-:" . helm-projectile))
    :config
    (helm-projectile-on))
  (leaf helm-bundle-show
    :doc "Bundle show with helm interface"
    :req "emacs-24" "helm-1.8.0"
    :tag "emacs>=24"
    :added "2020-08-31"
    :url "https://github.com/masutaka/emacs-helm-bundle-show"
    :emacs>= 24
    :ensure t
    :bind (("C-x y" . helm-bundle-show))
    :after helm)
  (leaf helm-ag
    :doc "The silver searcher with helm interface"
    :req "emacs-25.1" "helm-2.0"
    :tag "emacs>=25.1"
    :added "2020-08-31"
    :url "https://github.com/syohex/emacs-helm-ag"
    :emacs>= 25.1
    :ensure t
    :after helm
    :bind (("C-x C-g" . helm-ag)))
  :bind (("C-;" . helm-mini)
         ("C-x b" . helm-buffers-list)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         (helm-map
          ("C-h" . delete-backward-char)))
  :config
  (helm-mode t)
  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

(leaf session
  :doc "use variables, registers and buffer places across sessions"
  :tag "tools" "data" "desktop" "session management" "session"
  :added "2020-08-28"
  :url "http://emacs-session.sourceforge.net/"
  :ensure t
  :config
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 500)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))

  ;; これがないと file-name-history に500個保存する前に max-string に達する
  (setq session-globals-max-string 2048)

  ;; デフォルトでは30!
  (add-hook 'after-init-hook 'session-initialize))

(leaf whitespace-style
  :setq ((whitespace-style quote
                           (face tabs tab-mark spaces space-mark trailing space-before-tab space-after-tab::space))
         (whitespace-space-regexp . "\\(　+\\)")
         (whitespace-display-mappings quote
                                      ((space-mark 12288
                                                   [9633])
                                       (tab-mark 9
                                                 [187 9]))))
  :config
  (global-whitespace-mode t)
  (set-face-attribute 'whitespace-trailing nil :foreground "DeepPink" :underline t)
  (set-face-attribute 'whitespace-tab nil :foreground "labelColor" :underline t)
  (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :weight 'bold))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :added "2020-08-28"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 2)
           (company-selection-wrap-around . t))
  :bind ((company-active-map
          ("C-n" . company-select-next))
         (company-active-map
          ("C-p" . company-select-previous))
         (company-search-map
          ("C-n" . company-select-next))
         (company-search-map
          ("C-p" . company-select-previous))
         (company-active-map
          ("C-s" . company-filter-candidates))
         (company-active-map
          ("C-i" . company-complete-selection))
         (emacs-lisp-mode-map
          ("C-M-i" . company-complete)))
  :config
  (global-company-mode t)

  ;; 自動補完を offにしたい場合は, company-idle-delayを nilに設定する
  ;; auto-completeでいうところの ac-auto-start にあたる.
  ;; (custom-set-variables
  ;;  '(company-idle-delay nil))

  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :added "2020-08-28"
  :url "https://github.com/emacsorphanage/git-gutter"
  :emacs>= 24.3
  :ensure t
  :config
  (global-git-gutter-mode t)
  (add-hook 'ruby-mode-hook 'git-gutter-mode))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :added "2020-08-28"
  :emacs>= 25.1
  :ensure t
  :after git-commit with-editor
  :custom
  ((magit-status-buffer-switch-function quote switch-to-buffer)))

(leaf vc-annotate
  :preface
  (defun vc-annotate-open-pr-at-line nil
    (interactive)
    (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
           (rev (car rev-at-line)))
      (shell-command
       (concat "open-pr-from-commit " rev))))

  :bind ((vc-annotate-mode-map
          ("P" . vc-annotate-open-pr-at-line)))
  :require vc-annotate)

(leaf scss-custom
  :mode (("\\.scss$" . scss-mode)
         ("\\.css$" . scss-mode))

  :preface
  (defun scss-custom nil
    "scss-mode-hook"
    (and
     (set
      (make-local-variable 'css-indent-offset)
      2)
     (set
      (make-local-variable 'scss-compile-at-save)
      nil)))

  :config
  (add-hook 'scss-mode-hook
            '(lambda nil
               (scss-custom))))

(leaf elixir-mode
  :commands elixir-mode)

(leaf go
  :hook ((go-mode-hook . company-mode)
         (go-mode-hook . flycheck-mode))
  :setq ((gofmt-command . "goimports"))
  :config
  (add-hook 'go-mode-hook
            (lambda nil
              (add-hook 'before-save-hook 'gofmt-before-save)
              (local-set-key
               (kbd "M-.")
               'godef-jump)
              (set
               (make-local-variable 'company-backends)
               '(company-go))
              (company-mode)
              (setq tab-width 2))))

(leaf javascript
  :mode (("\\.js$" . rjsx-mode)
         ("\\.coffee$" . coffee-mode)
         ("Cakefile" . coffee-mode))
  :setq ((js-indent-level . 2)))

(leaf markdown
  :setq ((markdown-asymmetric-header . t)
         (markdown-header-scaling . t))
  :config
  (setq auto-mode-alist (append auto-mode-alist
                                (list
                                 '("\\.md" . markdown-mode)
                                 '("\\.markdown" . markdown-mode)))))

(leaf pandoc
  :when (executable-find "pandoc")
  :setq ((markdown-command . "pandoc -s --self-contained -t html5 -c ~/.pandoc/github-markdown.css")))

(leaf ruby-mode
  :doc "Major mode for editing Ruby files"
  :tag "builtin"
  :added "2020-08-28"
  :init
  (leaf ruby-block
    :tag "out-of-MELPA"
    :added "2020-08-28"
    :el-get juszczakn/ruby-block
    :require t)
  (leaf ruby-electric
    :doc "Minor mode for electrically editing ruby code"
    :tag "ruby" "languages"
    :added "2020-08-31"
    :url "https://github.com/ruby/elisp-ruby-electric"
    :ensure t
    :config (ruby-electric-mode t))
  :setq ((ruby-insert-encoding-magic-comment . nil))
  :config
  (autoload 'ruby-mode "ruby-mode"
    "Mode for editing ruby source files" t)
  (setq auto-mode-alist
        (append
         (list (cons "\\.rb$" 'ruby-mode))
         (list (cons "\\.rake$" 'ruby-mode))
         (list (cons "\\.racc$" 'ruby-mode))
         (list (cons "\\.jbuilder$" 'ruby-mode))
         (list (cons "Gemfile" 'ruby-mode))
         (list (cons "Capfile" 'ruby-mode))
         (list (cons "Rakefile" 'ruby-mode))
         auto-mode-alist))
  (setq interpreter-mode-alist
        (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
  (autoload 'run-ruby "inf-ruby"
    "Run an inferior Ruby process")

  ;; ruby-modeのインデントを改良する
  (setq ruby-deep-indent-paren-style nil)
  (defadvice ruby-indent-line (after unindent-closing-paren activate)
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset)))))

  (add-hook 'ruby-mode-hook
            '(lambda ()
               (ruby-electric-mode)
               (rspec-mode)
               (abbrev-mode 1)
               (ruby-block-mode t)
               (auto-highlight-symbol-mode t)
               (flycheck-mode t)))

  (setq ruby-electric-expand-delimiters-list nil)

  (setq ruby-block-highlight-toggle t)

  ;; シンボルをハイライト表示
  (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

  (eval-after-load 'rspec-mode
    '(rspec-install-snippets)))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :added "2020-08-28"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :config
  (flycheck-define-checker ruby-rubocop
    "A Ruby syntax and style checker using the RuboCop tool.

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
              (optional (id (one-or-more (not (any ":")))) ": ") (message)
              line-end)
     (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
            (optional (id (one-or-more (not (any ":")))) ": ") (message)
            line-end))
    :modes (enh-ruby-mode ruby-mode)))

(leaf open-junk-file
  :doc "Open a junk (memo) file to try-and-error"
  :tag "tools" "convenience"
  :added "2020-08-28"
  :url "http://www.emacswiki.org/cgi-bin/wiki/download/open-junk-file.el"
  :bind (("C-x j" . open-junk-file))
  :ensure t
  :setq ((open-junk-file-directory . "~/Dropbox/junk/%Y/%m-%d-%H%M%S.")))

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :added "2020-08-28"
  :url "http://web-mode.org"
  :emacs>= 23.1
  :ensure t
  :preface
  (defun my-web-mode-hook nil
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2))

  :mode (("\\.eex$" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.html?$" . web-mode))
  :hook ((web-mode-hook . my-web-mode-hook)))

(leaf ag
  :doc "A front-end for ag ('the silver searcher'), the C ack replacement."
  :req "dash-2.8.0" "s-1.9.0" "cl-lib-0.5"
  :added "2020-08-28"
  :ensure t
  :config
  (leaf wgrep-ag
    :doc "Writable ag buffer and apply the changes to files"
    :req "wgrep-2.3.2"
    :tag "extensions" "edit" "grep"
    :added "2020-08-28"
    :url "http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-ag.el"
    :ensure t
    :after wgrep
    :config
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)
    ;; agの検索結果バッファで"r"で編集モードに。
    ;; C-x C-sで保存して終了、C-x C-kで保存せずに終了
    (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)))

(leaf wdired
  :doc "Rename files editing their names in dired buffers"
  :tag "builtin"
  :added "2020-08-28"
  :config
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

(leaf all-the-icons
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3" "memoize-1.0.1"
  :tag "lisp" "convenient" "emacs>=24.3"
  :added "2020-08-28"
  :url "https://github.com/domtronn/all-the-icons.el"
  :emacs>= 24.3
  :ensure t
  :after memoize)

(leaf all-the-icons-dired
  :doc "Shows icons for each file in dired mode"
  :req "emacs-24.4" "all-the-icons-2.2.0"
  :tag "dired" "icons" "files" "emacs>=24.4"
  :added "2020-08-28"
  :url "https://github.com/jtbm37/all-the-icons-dired"
  :emacs>= 24.4
  :ensure t
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :added "2020-08-31"
  :emacs>= 24.1
  :ensure t)

(leaf make-file-executable
  :preface
  (defun make-file-executable nil
    "Make the file of this buffer executable, when it is a script source."
    (save-restriction
      (widen)
      (if (string= "#!"
                   (buffer-substring-no-properties 1
                                                   (min 3
                                                        (point-max))))
          (let ((name (buffer-file-name)))
            (or
             (equal 46
                    (string-to-char
                     (file-name-nondirectory name)))
             (let ((mode (file-modes name)))
               (set-file-modes name
                               (logior mode
                                       (logand
                                        (/ mode 4)
                                        73)))
               (message
                (concat "Wrote " name " (+x)"))))))))

  :hook ((after-save-hook . make-file-executable)))

(leaf smart-jump
  :doc "Smart go to definition."
  :req "emacs-25.1" "dumb-jump-0.5.1"
  :tag "tools" "emacs>=25.1"
  :added "2020-08-28"
  :url "https://github.com/jojojames/smart-jump"
  :emacs>= 25.1
  :ensure t
  :after dumb-jump
  :setq ((dumb-jump-default-project . "")
         (dumb-jump-force-searcher . 'ag))
  :config
  (dumb-jump-mode t)
  (smart-jump-setup-default-registers))

(leaf migemo
  :doc "Japanese incremental search through dynamic pattern expansion"
  :req "cl-lib-0.5"
  :added "2020-08-28"
  :url "https://github.com/emacs-jp/migemo"
  :ensure t
  :setq ((migemo-command . "cmigemo")
         (migemo-options . '("-q" "--emacs"))
         (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
         (migemo-user-dictionary . nil)
         (migemo-regex-dictionary . nil)
         (migemo-coding-system . 'utf-8-unix))
  :config
  (when (file-exists-p "/usr/local/share/migemo/utf-8/migemo-dict")
    (load-library "migemo")
    (migemo-init)))

(leaf keyfreq
  :doc "track command frequencies"
  :req "cl-lib-0.5"
  :added "2020-08-31"
  :ensure t
  :setq ((keyfreq-file . "~/Dropbox/dotfiles/local/emacs.keyfreq"))
  :config
  (keyfreq-autosave-mode 1)
  (keyfreq-mode 1))

(leaf recentf
  :doc "setup a menu of recently opened files"
  :tag "builtin"
  :added "2020-08-31"
  :setq ((recentf-max-menu-items . 200)
         (recentf-max-saved-items . 3000))
  :config
  (recentf-mode t))

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
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(provide 'init)
