(defun alchemist--get-tramp-file-name (file)
  (if (string-match ":" default-directory)
      (concat (string-join (butlast (split-string default-directory ":")) ":") ":" file)
    file))

(defun alchemist-goto-filter (_process output)
  (with-local-quit
    (setq alchemist-goto-filter-output (cons output alchemist-goto-filter-output))
    (when (alchemist-server-contains-end-marker-p output)
      (let* ((output (alchemist-server-prepare-filter-output alchemist-goto-filter-output))
	     (file output))
	(setq alchemist-goto-filter-output nil)
	(funcall alchemist-goto-callback (alchemist--get-tramp-file-name file))))))
