;; (package-initialize)

(prefer-coding-system 'utf-8)

(load "~/.emacs.d/packages.el")

(when (not (eq window-system 'w32))
  (exec-path-from-shell-initialize))

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
   (quote
    ("f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" default)))
 '(deft-file-naming-rules
    (quote
     ((noslash . "-")
      (nospace . "-")
      (case-fn . downcase))))
 '(deft-markdown-mode-title-level 1)
 '(deft-use-filter-string-for-filename t)
 '(delete-old-versions t)
 '(fill-column 120)
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kept-new-versions 20)
 '(kept-old-versions 5)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (shell . t)
     (dot . t)
     (plantuml . t)
     (python . t)
     (sql . t))))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (groovy-mode dockerfile-mode poly-markdown csharp-mode graphviz-dot-mode vagrant-tramp kubernetes-helm kubernetes-tramp centaur-tabs docker-tramp helm-projectile helm powershell default-text-scale csv-mode deft dot-mode exec-path-from-shell yasnippet yaml-mode which-key uuidgen restclient neotree magit kibit-helper flycheck-pos-tip flycheck-clojure flycheck monokai-theme clj-refactor cider beacon auto-highlight-symbol auto-complete aggressive-indent)))
 '(python-indent-offset 2)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(tramp-remote-shell-executable "sh")
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(whitespace-style
   (quote
    (face trailing tabs indentation space-after-tab space-before-tab tab-mark)))
 '(woman-bold-headings t)
 '(woman-fill-column 100)
 '(woman-fill-frame t))

(put 'narrow-to-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Fira Code")))))

(server-start)

(require 'auto-complete)
(require 'auto-highlight-symbol)

(load-theme 'monokai t)

;; Set frame size
(when window-system
  (let ((target-width (truncate (/ (* 0.85 (display-pixel-width))
                                   (window-font-width))))
        (target-height (truncate (/ (* 0.85 (display-pixel-height))
                                    (window-font-height)))))
    (set-frame-size (window-frame) target-width target-height)
    (set-frame-position (window-frame) 4 4)))

;; Modes
(global-auto-highlight-symbol-mode t)
(global-flycheck-mode t)
(global-linum-mode t)
(global-whitespace-mode t)
(beacon-mode t)
(centaur-tabs-mode t)
(default-text-scale-mode t)
(global-undo-tree-mode t)
(helm-mode t)
(projectile-mode t)
(yas-global-mode t)

;; Set modes to files
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Autocomplete config
(define-key ac-completing-map [return] nil) ; no enter (1.)
(define-key ac-completing-map "\r" nil) ; no enter (2.)
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete

;; Centaur Tabs config
(load "~/.emacs.d/centaur-tabs.el")

;; Deft config
(load "~/.emacs.d/deft.el")

;; Eshell config
(setenv "EDITOR" "emacsclient")
(setenv "GIT_EDITOR" "emacsclient")
(defalias 'ff 'find-file)
(global-set-key (kbd "C-x c") 'eshell)

;; Evil mode
(global-set-key (kbd "C-x x") 'evil-mode)

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

;; Helm-mode config
(global-set-key (kbd "C-x C-n") 'helm-projectile)

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

;; Yaml-mode config
(load "~/.emacs.d/yaml-mode.el")

;; hooks
(mapc (lambda (hook) (add-hook hook 'paredit-mode))
      '(clojure-mode-hook
        emacs-lisp-mode-hook))
(mapc (lambda (hook) (add-hook hook 'alchemist-mode))
      '(elixir-mode-hook
        company-mode))

(require 'jq-mode)
(add-hook 'restclient-mode-hook
          #'(lambda ()
              (require 'restclient-jq)
              (setq jq-interactive-command (locate-file "jq" exec-path))))

;; Machine-specific profile
(if (file-exists-p "~/.emacs-local.d/init.el")
    (load "~/.emacs-local.d/init.el"))

