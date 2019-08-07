(setq ivy-fixed-height-minibuffer t
      ivy-height 20
      dumb-jump-selector 'ivy
      smex-history-length 35
      smex-completion-method 'ivy
      counsel-yank-pop-separator "\n-------\n")

(when (require 'all-the-icons-ivy nil t)
  (dolist (command '(counsel-projectile-switch-project
                     counsel-ibuffer))
    (add-to-list 'all-the-icons-ivy-buffer-commands command))
  (all-the-icons-ivy-setup))
