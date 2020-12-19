(require 'alchemist)
(load "~/.emacs.d/alchemist-fixes/alchemist-goto.el")
(load "~/.emacs.d/alchemist-fixes/alchemist-iex.el")
(load "~/.emacs.d/alchemist-fixes/alchemist-server.el")

(mapc (lambda (hook) (add-hook hook 'company-mode))
      '(elixir-mode-hook
        alchemist-mode-hook
        alchemist-iex-mode-hook))

(add-hook 'alchemist-mode-hook (lambda ()
                                 (setq show-trailing-whitespace t)))

(define-key alchemist-mode-map (kbd "C-c C-k") 'alchemist-iex-compile-this-buffer)
(define-key alchemist-mode-map [(meta tab)] 'alchemist-company)


