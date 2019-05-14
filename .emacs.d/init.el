
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(require 'cask "/usr/local/opt/cask/cask.el")
;(require 'cask)
(cask-initialize)

(pallet-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-reuse-buffers (quote nil))
 '(ag-reuse-window (quote nil))
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(apib-drafter-executable "aglio")
 '(column-number-mode 1)
 '(custom-safe-themes
   (quote
    ("5d3e0746023fc5e246eb3e0e48c1ccb5ce0387fc4273896c6cf02ee349c2eba8" default)))
 '(dumb-jump-default-project "")
 '(dumb-jump-force-searcher (quote ag))
 '(dumb-jump-mode t)
 '(global-anzu-mode t)
 '(global-auto-revert-mode 1)
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(init-loader-show-log-after-init (quote error-only))
 '(keyfreq-autosave-mode 1)
 '(keyfreq-file "~/Dropbox/dotfiles/local/emacs.keyfreq")
 '(keyfreq-mode 1)
 '(line-number-mode 1)
 '(neo-show-hidden-files t)
 '(neo-theme (quote icons))
 '(open-junk-file-format "~/Dropbox/junk/%Y/%m-%d-%H%M%S." t)
 '(package-selected-packages
   (quote
    (smart-jump lsp-ruby eglot poly-erb poly-markdown session ruby-electric all-the-icons-dired neotree doom-themes which-key web-mode magit color-identifiers-mode google-translate twittering-mode dracula-theme apib-mode migemo lua-mode package-build shut-up epl git commander f s helm-rb keyfreq color-theme-modern elpy py-autopep8 highlight-symbol mark-multiple expand-region bind-key company-go dash yaml-mode wgrep-ag smart-newline sequential-command scss-mode scratch-ext ruby-end ruby-block rubocop rspec-mode rinari pallet osx-dictionary open-junk-file multicolumn markdown-mode js2-mode init-loader helm-projectile helm-ls-git helm-ghq helm-bundle-show helm-ag-r helm-ag haskell-mode haml-mode go-autocomplete gitignore-mode github-browse-file gitconfig-mode git-gutter git-gutter+ gh flycheck fish-mode exec-path-from-shell elscreen elixir-mode dumb-jump dired+ color-moccur coffee-mode auto-highlight-symbol anzu ag 2048-game)))
 '(recentf-max-menu-items 200)
 '(recentf-max-saved-items 3000)
 '(recentf-mode t)
 '(ruby-insert-encoding-magic-comment nil)
 '(scratch-ext-log-directory "~/Dropbox/scratch/")
 '(send-mail-function (quote smtpmail-send-it))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(smart-newline-mode t t)
 '(transient-mark-mode t)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(which-key-mode t)
 '(which-key-setup-side-window-right t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-background-face ((t (:background "gray10"))))
 '(elscreen-tab-control-face ((t (:background "gray10" :foreground "gray60"))))
 '(elscreen-tab-current-screen-face ((t (:background "gray75" :foreground "black"))))
 '(elscreen-tab-other-screen-face ((t (:background "gray30" :foreground "gray80")))))

;; init loader
(init-loader-load "~/.emacs.d/inits")

;; after init
(add-hook 'after-init-hook
  (lambda ()
    ;; show init time
    (message "init time: %.3f sec"
             (float-time (time-subtract after-init-time before-init-time)))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
