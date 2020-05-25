(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(require 'package)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(aggressive-indent
                   alchemist
                   auto-complete
                   auto-highlight-symbol
                   beacon
                   cider
                   clojure-mode
                   clj-refactor
                   elixir-mode
                   evil
                   flycheck
                   flycheck-clojure
                   flycheck-pos-tip
                   kibit-helper
                   magit
                   markdown-mode
                   monokai-theme
                   multiple-cursors
                   neotree
                   paredit
                   projectile
                   restclient
                   uuidgen
                   which-key
                   yaml-mode))
  (unless (package-installed-p package)
    (package-install package)))

