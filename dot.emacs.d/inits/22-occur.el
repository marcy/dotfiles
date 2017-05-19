(require 'color-moccur)

(setq dmoccur-exclusion-mask
      (append '("\\~$" "\\.svn\\/\*" "\\.log" "\\.rsync_cache\\/\*" "\\.git\\/\*") dmoccur-exclusion-mask))

(setq moccur-use-migemo nil)
(defun toggle-moccur-use-migemo ()
  (interactive)
  (setq moccur-use-migemo (if moccur-use-migemo
                              nil
                            t))
  (message "%s" moccur-use-migemo))

(load "moccur-edit")
