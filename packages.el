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
                   blacken
                   centaur-tabs
                   cider
                   clojure-mode
                   clj-refactor
                   csharp-mode
                   csv-mode
                   default-text-scale
                   deft
                   docker-tramp
                   dockerfile-mode
                   dot-mode
                   elixir-mode
                   elpy
                   evil
                   exec-path-from-shell
                   flycheck
                   flycheck-clojure
                   flycheck-pos-tip
                   graphviz-dot-mode
                   groovy-mode
                   helm-projectile
                   jq-mode
                   kibit-helper
                   kubernetes-helm
                   kubernetes-tramp
                   magit
                   markdown-mode
                   monokai-theme
                   multiple-cursors
                   neotree
                   paredit
                   poly-markdown
                   powershell
                   python-black
                   restclient
                   ripgrep
                   uuidgen
                   vagrant-tramp
                   which-key
                   yaml-mode))
  (unless (package-installed-p package)
    (package-install package)))

