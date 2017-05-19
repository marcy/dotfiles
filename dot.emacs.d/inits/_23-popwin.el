(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

(push '("*rspec-compilation*" :regexp t) popwin:special-display-config)
