(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(require 'package)

(add-to-list 'load-path "~/.emacs.d/more-packages")
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(alchemist
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
                   jq-mode
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

