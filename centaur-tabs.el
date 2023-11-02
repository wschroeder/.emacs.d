(require 'centaur-tabs)

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules."
  (list
   (cond
    ((or
      (member (buffer-name) '("*Completions*"
                              "*Messages*"
                              "*Backtrace*"
                              "*Apropos*"
                              "*alchemist-server*"
                              "*Flymake log*"
                              "*elpy-virtualenv*"
                              "*Async-native-compile-log*"))
      (string-prefix-p "*vim*" (buffer-name))
      (string-prefix-p "*cider-" (buffer-name))
      (string-prefix-p "*nrepl-" (buffer-name))
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
    ((or (member (buffer-name) '("*inferior-lisp*"))
         (string-prefix-p "*sly-" (buffer-name)))
     "Sly")
    ((or (memq major-mode '(ovpn-mode))
         (string-suffix-p ".ovpn" (buffer-name)))
     "Openvpn")
    ((string-match-p "\\.md\\[.*\\]" (buffer-name))
     "Poly-Markdown")
    (t "Primary"))))

(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "♫")
(setq centaur-tabs-cycle-scope 'tabs)

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
(global-set-key (kbd "C-x <right>") 'centaur-tabs-move-current-tab-to-right)
(global-set-key (kbd "C-x <left>") 'centaur-tabs-move-current-tab-to-left)
