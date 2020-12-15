(defun alchemist-server-start-in-env (env)
  "Start an Alchemist server with the ENV."
  (let* ((process-name (alchemist-server-process-name))
         (default-directory (if (string= process-name "alchemist-server")
                                default-directory
                              process-name))
         (process (start-file-process process-name "*alchemist-server*" alchemist-execute-command alchemist-server env)))
    (set-process-query-on-exit-flag process nil)
    (alchemist-server--store-process process)))
