(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
        ;; ("melpa-stable" . "http://stable.melpa.org/packages/")))

(require 'package)

(add-to-list 'load-path "~/.emacs.d/more-packages")
(package-initialize)

(defvar need-package-update t)

(dolist (package '(alchemist
                   auto-complete
                   auto-highlight-symbol
                   beacon
                   blacken
                   centaur-tabs
                   cider
                   clojure-mode
                   clj-refactor
                   counsel-projectile
                   csharp-mode
                   csv-mode
                   default-text-scale
                   deft
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
                   ivy
                   jq-mode
                   kibit-helper
                   magit
                   markdown-mode
                   monokai-theme
                   multiple-cursors
                   nhexl-mode
                   neotree
                   paredit
                   php-mode
                   poly-markdown
                   powershell
                   pyenv-mode
                   pylint
                   python-black
                   restclient
                   ripgrep
                   uml-mode
                   undo-tree
                   uuidgen
                   virtualenvwrapper
                   which-key
                   yaml-mode))
  (unless (package-installed-p package)
    (when need-package-update
      (package-refresh-contents)
      (setf need-package-update nil))
    (package-install package)))
