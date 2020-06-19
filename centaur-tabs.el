(require 'centaur-tabs)

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules."
  (list
   (cond
    ((or
      (member (buffer-name) '("*Completions*"
                              "*Messages*"
                              "*Backtrace*"
                              "*Apropos*"))
      (memq major-mode '(magit-process-mode
                         magit-status-mode
                         magit-diff-mode
                         magit-log-mode
                         magit-file-mode
                         magit-blob-mode
                         magit-blame-mode
                         helm-mode
                         )))
     "Special")
    ((or (memq major-mode '(ovpn-mode))
         (string-suffix-p ".ovpn" (buffer-name)))
     "Openvpn")
    (t "Primary"))))

(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "♫")

(global-set-key (kbd "M-k") 'centaur-tabs-backward)
(global-set-key (kbd "M-j") 'centaur-tabs-forward)
(global-set-key (kbd "M-c") #'(lambda ()
                                (interactive)
                                (kill-buffer (buffer-name))))
(global-set-key (kbd "M-n") #'(lambda ()
                                (interactive)
                                (switch-to-buffer (generate-new-buffer (concat "*"
                                                                               (number-to-string (random 10000))
                                                                               "*")))))
