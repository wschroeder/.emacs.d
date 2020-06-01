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
                   centaur-tabs
                   cider
                   clojure-mode
                   clj-refactor
                   csv-mode
                   default-text-scale
                   deft
                   docker-tramp
                   dot-mode
                   elixir-mode
                   evil
                   exec-path-from-shell
                   flycheck
                   flycheck-clojure
                   flycheck-pos-tip
                   helm-projectile
                   jq-mode
                   kibit-helper
                   magit
                   markdown-mode
                   monokai-theme
                   multiple-cursors
                   neotree
                   paredit
                   powershell
                   restclient
                   uuidgen
                   which-key
                   yaml-mode))
  (unless (package-installed-p package)
    (package-install package)))

