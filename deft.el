(setq deft-default-extension "md")

(defun new-log-for-today ()
  (interactive)
  (deft-filter-clear)
  (let ((deft-default-extension "org")
        (today-name (concat "TODO:" (format-time-string "%F"))))
    (execute-kbd-macro (kbd today-name))
    (if (length (deft-current-files))
        (deft-complete)
      (deft-new-file))))

(defun setup-deft-mode ()
  (local-set-key (kbd "C-c C-p") 'new-log-for-today))

(add-hook 'deft-mode-hook 'setup-deft-mode)
