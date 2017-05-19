(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)

  (require 'go-autocomplete)
  (require 'auto-complete-config)

  (ac-config-default)

  (setq ac-auto-start 3)

  (define-key ac-completing-map "\C-n" 'ac-next)
  (define-key ac-completing-map "\C-p" 'ac-previous))
