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
    (rspec-mode)
    (abbrev-mode 1)
    (ruby-electric-mode t)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)
    (ruby-block-mode t)
    (auto-highlight-symbol-mode t)
    (flycheck-mode t)))

(setq ruby-electric-expand-delimiters-list nil)

(setq ruby-block-highlight-toggle t)

;; シンボルをハイライト表示
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(require 'flycheck)
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
  :modes (enh-ruby-mode ruby-mode))
