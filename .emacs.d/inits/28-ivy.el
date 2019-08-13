(setq ivy-height 30)

(setq dumb-jump-selector 'ivy)

(when (require 'all-the-icons-ivy nil t)
  (dolist (command '(counsel-projectile-switch-project
                     counsel-ibuffer))
    (add-to-list 'all-the-icons-ivy-buffer-commands command))
  (all-the-icons-ivy-setup))
