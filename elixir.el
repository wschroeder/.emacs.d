(mapc (lambda (hook) (add-hook hook 'alchemist-mode))
      '(elixir-mode-hook
        company-mode))

