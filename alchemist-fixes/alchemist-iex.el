(defun alchemist-iex-compile-this-buffer ()
  "Compiles the current buffer in the IEx process."
  (interactive)
  (let* ((path (if (alchemist-project-p)
		   (format "%s/_build/dev/" (alchemist-project-root))
		 "."))
	 (str (format "c(\"%s\", \"%s\")" (file-local-name (buffer-file-name)) (file-local-name path))))
    (alchemist-iex--send-command (alchemist-iex-process) str)))

