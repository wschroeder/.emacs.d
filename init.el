;; (package-initialize)

(prefer-coding-system 'utf-8)

(load "~/.emacs.d/packages.el")

(when (not (eq window-system 'w32))
  (exec-path-from-shell-initialize))

(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"System Events\" to tell process \"emacs\" to set frontmost to true"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))

(if (not (file-exists-p "~/.emacs.d/flycheck/"))
    (make-directory "~/.emacs.d/flycheck/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (list (list "^.*/\\([^/]+\\)$" "~/.emacs.d/backups/\\1")))
 '(backup-by-copying t)
 '(backup-directory-alist (list (cons "." "~/.emacs.d/backups")))
 '(beacon-color "#ff00ff")
 '(centaur-tabs-hide-tabs-hooks nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" default))
 '(deft-file-naming-rules '((noslash . "-") (nospace . "-") (case-fn . downcase)))
 '(deft-markdown-mode-title-level 1)
 '(deft-use-filter-string-for-filename t)
 '(delete-old-versions t)
 '(elpy-rpc-ignored-buffer-size 200000)
 '(elpy-rpc-timeout 10)
 '(evil-ex-search-vim-style-regexp t)
 '(evil-shift-width 2)
 '(evil-undo-system 'undo-tree)
 '(fill-column 120)
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-temp-prefix "~/.emacs.d/flycheck/flycheck")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kept-new-versions 20)
 '(kept-old-versions 5)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (dot . t)
     (plantuml . t)
     (python . t)
     (sql . t)))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   '(evil php-mode uml-mode vagrant nhexl-mode elixir-mode pylint virtualenvwrapper pyenv-mode ripgrep blacken python-black elpy groovy-mode dockerfile-mode poly-markdown csharp-mode graphviz-dot-mode kubernetes-helm centaur-tabs helm-projectile helm powershell default-text-scale csv-mode deft dot-mode exec-path-from-shell yasnippet yaml-mode which-key uuidgen restclient neotree magit kibit-helper flycheck-pos-tip flycheck-clojure flycheck monokai-theme clj-refactor cider beacon auto-highlight-symbol auto-complete aggressive-indent))
 '(python-indent-offset 2)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(tramp-remote-shell-executable "sh")
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(version-control t)
 '(whitespace-style
   '(face trailing tabs indentation space-after-tab space-before-tab tab-mark))
 '(woman-bold-headings t)
 '(woman-fill-column 100)
 '(woman-fill-frame t))

(put 'narrow-to-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "FiraCode Nerd Font")))))

(server-start)

(require 'auto-complete)
(require 'auto-highlight-symbol)

(load-theme 'monokai t)

;; Set frame size
(when window-system
  (cl-destructuring-bind (x y width height)
    (frame-monitor-workarea)
    (let ((target-width (truncate (/ (* 0.85 width)
                                     (window-font-width))))
          (target-height (truncate (/ (* 0.85 height)
                                      (window-font-height)))))
      (set-frame-size (window-frame) target-width target-height)
      (set-frame-position (window-frame) (+ x 4) (+ y 4)))))

;; Modes
(global-auto-highlight-symbol-mode t)
(global-flycheck-mode t)
(global-display-line-numbers-mode t)
(global-whitespace-mode t)
(beacon-mode t)
(centaur-tabs-mode t)
(default-text-scale-mode t)
(global-undo-tree-mode t)
(ivy-mode t)
(projectile-mode t)
(yas-global-mode t)

;; Set modes to files
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Alchemist config
(load "~/.emacs.d/alchemist.el")

;; Autocomplete config
(define-key ac-completing-map [return] nil) ; no enter (1.)
(define-key ac-completing-map "\r" nil) ; no enter (2.)
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete

;; blacken
(when (and (executable-find "pyenv")
           (executable-find "black"))
  (load "blacken")
  (load "python")
  (setq blacken-only-if-project-is-blackened t)
  (add-hook 'python-mode-hook 'blacken-mode))

;; Centaur Tabs config
(load "~/.emacs.d/centaur-tabs.el")

;; Deft config
(load "~/.emacs.d/deft.el")

;; Elpy
(when (executable-find "pyenv")
  (load "pyenv-mode")
  (when (file-exists-p "~/Repositories/elasticbox/src")
    (setenv "PYTHONPATH" (expand-file-name "~/Repositories/elasticbox/src")))
  (pyenv-mode)
  (elpy-enable)
  (let ((workon-home (expand-file-name "~/.pyenv/versions")))
    (setenv "WORKON_HOME" workon-home)
    (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home)))

;; Eshell config
(setenv "EDITOR" "emacsclient")
(setenv "GIT_EDITOR" "emacsclient")
(defalias 'ff 'find-file)
(global-set-key (kbd "C-x c") 'eshell)

;; Evil mode
(global-set-key (kbd "C-x x") 'evil-mode)

(require 'evil)
(mapc (lambda (exclude-vim-from-this-mode)
        (evil-set-initial-state exclude-vim-from-this-mode 'emacs))
      '(lisp-mode
        elisp-mode
        emacs-lisp-mode
        magit-mode
        package-menu-mode
        deft-mode
        Info-mode
        eshell-mode))

(mapc (lambda (function-to-vimmify)
        (advice-add function-to-vimmify :around
                    #'(lambda (fn &rest args)
                        (let ((table (copy-syntax-table (syntax-table))))
                          (modify-syntax-entry ?_ "w" table)
                          (with-syntax-table table
                            (apply fn args))))))
      '(evil-forward-word-begin
        evil-forward-word-end
        evil-backward-word-begin
        evil-backward-word-end
        evil-select-an-object
        evil-select-inner-object))

(require 'markdown-mode)
(evil-make-overriding-map markdown-mode-map 'normal)

;; Fun functions stolen from https://github.com/sroccaserra/emacs/blob/master/tools.el
(defmacro comment (&rest body)
  "Ignores body, yields nil"
  nil)

(defun sequence (maybe-seq)
  "Returns the value wrapped in a sequence if it is not a sequence already."
  (if (sequencep maybe-seq) maybe-seq
    (list maybe-seq)))

(defmacro -> (x &optional form &rest more)
  (cond ((not (null more))
         `(-> (-> ,x ,form) ,@more))
        ((not (null form))
         (if (sequencep form)
             `(,(first form) ,x ,@(rest form))
           (list form x)))
        (t x)))

(defmacro ->> (x form &rest more)
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(,(first form) ,@(rest form) ,x)
             (list form x)))))

;; ibuffer instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ivy-mode config
(global-set-key (kbd "C-x C-n") 'counsel-projectile)

;; Lisp interaction mode
(define-key lisp-interaction-mode-map (kbd "C-c C-j") 'eval-print-last-sexp)

;; Multiple Cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-;") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this-symbol)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-symbol)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Neotree
(global-set-key [f8] 'neotree-toggle)

;; Org-mode config
(load "~/.emacs.d/org-mode.el")

;; ripgrep
(defalias 'rg 'ripgrep-regexp)

;; Roswell Lisp
(when (executable-find "ros")
  (when (not (file-exists-p "~/.roswell/helper.el"))
    (shell-command "ros install sly"))
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq inferior-lisp-program "ros -Q run"))

;; Yaml-mode config
(load "~/.emacs.d/yaml-mode.el")

;; hooks
(mapc (lambda (hook) (add-hook hook 'paredit-mode))
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        sly-mrepl-mode-hook))
(mapc (lambda (hook) (add-hook hook 'company-mode))
      '(elixir-mode-hook
        alchemist-mode-hook
        alchemist-iex-mode-hook))

(require 'jq-mode)
(add-hook 'restclient-mode-hook
          #'(lambda ()
              (require 'restclient-jq)
              (setq jq-interactive-command (locate-file "jq" exec-path))))

;; Machine-specific profile
(if (file-exists-p "~/.emacs-local.d/init.el")
    (load "~/.emacs-local.d/init.el"))

