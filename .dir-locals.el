((nil . ((eval . (setq-local
                  org-roam-directory (file-name-concat
				      (expand-file-name (locate-dominating-file
                                                         default-directory ".dir-locals.el")))))
         (eval . (setq-local
                  org-roam-db-location (expand-file-name "org-roam.db"
                                                         org-roam-directory)))
	 (eval . (org-roam-db-autosync-mode))
	 )))
