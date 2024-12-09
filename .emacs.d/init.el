(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(custom-set-variables
 '(gnutls-algorithm-priority "normal:-vers-tls1.3"))

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
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs))

(leaf server
  :doc "Lisp code for GNU Emacs running as server process"
  :tag "builtin"
  :added "2020-09-02"
  :config
  (server-start))

;; ファイル保存時に行末の空行消す
(leaf leaf-convert
  :hook ((before-save-hook . delete-trailing-whitespace)))

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf tab-bar
  :doc "frame-local tabs with named persistent window configurations"
  :tag "builtin"
  :added "2024-12-09"
  :config
  (global-unset-key (kbd "C-t"))
  (define-prefix-command 'my-tab-bar-map)
  (global-set-key (kbd "C-t") 'my-tab-bar-map)

  (define-key my-tab-bar-map (kbd "c") 'tab-new)
  (define-key my-tab-bar-map (kbd "k") 'tab-close)
  (define-key my-tab-bar-map (kbd "n") 'tab-next)
  (define-key my-tab-bar-map (kbd "p") 'tab-previous)
  (dotimes (i 9)
    (define-key my-tab-bar-map (kbd (number-to-string (1+ i)))
                `(lambda () (interactive) (tab-bar-select-tab ,(1+ i)))))

  (custom-set-faces
   '(tab-bar ((t (:background "gray10"))))
   '(tab-bar-tab ((t (:background "gray75" :foreground "black" :box nil))))
   '(tab-bar-tab-inactive ((t (:background "gray30" :foreground "gray80" :box nil))))))

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

(leaf font
  :when (eq window-system 'mac)
  :config
  (let* ((size 16)
         (asciifont "Cica")
         (jpfont "Cica")
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
    (setq moccur-use-migemo (if moccur-use-migemo nil t))
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
         (whitespace-display-mappings quote ((space-mark 12288 [9633])
                                             (tab-mark 9 [187 9]))))
  :config
  (global-whitespace-mode t)
  (set-face-attribute 'whitespace-trailing nil :foreground "DeepPink" :underline t)
  (set-face-attribute 'whitespace-tab nil :foreground "grey20" :background 'unspecified :underline t)
  (set-face-attribute 'whitespace-space nil :foreground "GreenYellow" :weight 'bold))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2020-11-02"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :after spinner markdown-mode lv
  :init
  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :added "2020-11-02"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 26.1
    :ensure t
    :after lsp-mode markdown-mode))

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
  :bind ((company-active-map ("C-n" . company-select-next))
         (company-active-map ("C-p" . company-select-previous))
         (company-search-map ("C-n" . company-select-next))
         (company-search-map ("C-p" . company-select-previous))
         (company-active-map ("C-s" . company-filter-candidates))
         (company-active-map ("C-i" . company-complete-selection))
         (emacs-lisp-mode-map ("C-M-i" . company-complete)))
  :config
  (global-company-mode t)
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

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-25.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=25.1"
  :added "2020-09-23"
  :url "https://jblevins.org/projects/markdown-mode/"
  :emacs>= 25.1
  :ensure t
  :setq ((markdown-asymmetric-header . t)
         (markdown-header-scaling . t))
  :config
  (setq auto-mode-alist (append auto-mode-alist
                                (list
                                 '("\\.md" . markdown-mode)
                                 '("\\.markdown" . markdown-mode)))))

(leaf rspec-mode
  :doc "Enhance ruby-mode for RSpec"
  :req "ruby-mode-1.0" "cl-lib-0.4"
  :tag "ruby" "rspec"
  :added "2020-10-07"
  :url "http://github.com/pezra/rspec-mode"
  :ensure t
  :after ruby-mode)

(leaf haml-mode
  :doc "Major mode for editing Haml files"
  :req "emacs-24" "cl-lib-0.5"
  :tag "html" "languages" "markup" "emacs>=24"
  :added "2021-01-14"
  :url "https://github.com/nex3/haml-mode"
  :emacs>= 24
  :ensure t)

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
               (flycheck-mode t)))
  (add-hook 'ruby-mode-hook #'lsp)

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

(leaf ag
  :doc "A front-end for ag ('the silver searcher'), the C ack replacement."
  :req "dash-2.8.0" "s-1.9.0" "cl-lib-0.5"
  :added "2020-08-28"
  :ensure t
  :bind (("C-x C-g" . ag))
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
  :init
  (leaf all-the-icons-dired
    :doc "Shows icons for each file in dired mode"
    :req "emacs-24.4" "all-the-icons-2.2.0"
    :tag "dired" "icons" "files" "emacs>=24.4"
    :added "2020-08-28"
    :url "https://github.com/jtbm37/all-the-icons-dired"
    :emacs>= 24.4
    :ensure t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :added "2020-08-31"
  :emacs>= 24.1
  :ensure t)

(leaf recentf
  :doc "setup a menu of recently opened files"
  :tag "builtin"
  :added "2020-08-31"
  :setq ((recentf-max-menu-items . 200)
         (recentf-max-saved-items . 3000))
  :config
  (recentf-mode t))


(leaf leaf-convert
  :config
  (global-auto-revert-mode 1))

(leaf-keys (("C-c h" . help-for-help)
            ("C-x C-c" . server-edit)
            ("C-h" . delete-backward-char)
            ("C-@" . mark-word)
            ("C-x l" . goto-line)))

(leaf leaf-convert
  :setq ((default-directory . "~/")))

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
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
