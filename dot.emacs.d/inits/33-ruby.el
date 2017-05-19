(custom-set-variables
 '(ruby-insert-encoding-magic-comment nil))

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

(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)))

(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; シンボルをハイライト表示
(require 'auto-highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

(global-set-key (kbd "") 'highlight-symbol-at-point)

(add-hook 'ruby-mode-hook 'auto-highlight-symbol-mode)

(require 'rspec-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
