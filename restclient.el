(defun setup-restclient ()
  (require 'restclient-jq)
  (setq jq-interactive-command (locate-file "jq" exec-path)))

(add-hook 'restclient-mode-hook 'setup-restclient)
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))
