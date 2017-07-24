;;Allow us to load custom Lisp-------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp/")
;;-----------------------------------------------------------

;;Utils------------------------------------------------------
(defun my/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))
;;----------------------------------------------------------

;;package archives------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(defvar my-packages)
(setq my-packages
      '(cider queue spinner clojure-mode clojure-mode-extra-font-locking
              clojure-mode ido-ubiquitous ido-completing-read+ magit git-commit
              with-editor magit-popup async paredit projectile rainbow-delimiters
              smex tagedit s exec-path-from-shell flycheck-kotlin flycheck-rust
              flycheck-scala-sbt flycheck-swift3 flycheck dash flymake-rust
              flymake-easy graphql-mode json-mode json-reformat json-snatcher
              kotlin-mode pkg-info epl rust-mode sbt-mode scala-mode seq
              swift-mode toml-mode typescript-mode wakatime-mode yaml-mode))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))
;;----------------------------------------------------------

;;Make sure we get the correct environment variables---------
(exec-path-from-shell-initialize)
;;-----------------------------------------------------------

;;ELisp-------------------------------------------------------
;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook           #'rainbow-delimiters-mode)
;;-------------------------------------------------------------

;;JS mode/HTML mode---------------------------------------------------
(require 'js)
(setq js-indent-level 2)
(add-hook 'js-mode-hook #'subword-mode)
(add-hook 'js-mode-hook #'rainbow-delimiters-mode)
(add-hook 'html-mode-hook 'subword-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
;;----------------------------------------------------------

;;TypeScript------------------------------------------------
(add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;;----------------------------------------------------------

;;Python mode-----------------------------------------------
(setq-default python-indent-offset 2)
;;----------------------------------------------------------

;;Clojure---------------------------------------------------
(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'cider-mode)


(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (setq inferior-lisp-program "lein repl")
;;             (font-lock-add-keywords
;;              nil
;;              '(("(\\facts?\\)"
;;                 (1 font-lock-keyword-face))
;;                ("(\\background?\\)"
;;                 (1 font-lock-keyword-face))))
;;             (define-clojure-indent (fact 1))
;;             (define-clojure-indent (facts 1))))

(require 'cider-eldoc)
(add-hook 'cider-mode-hook 'eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . ruby-mode))
;;----------------------------------------------------------

;;Shell----------------------------------------------------
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
;;----------------------------------------------------------
 
;;Wakatime--------------------------------------------------
(require 'wakatime-mode)
(setq wakatime-api-key (getenv "WAKATIME_KEY"))
(setq wakatime-cli-path "/usr/bin/wakatime")
(global-wakatime-mode)
;;----------------------------------------------------------

;;customize-------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;----------------------------------------------------------

;;startup---------------------------------------------------
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
;;----------------------------------------------------------

;;IDO-------------------------------------------------------
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-virtual-buffers t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(ido-mode t)
(ido-ubiquitous-mode 1)
;;----------------------------------------------------------

;;SMEX------------------------------------------------------
(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(global-set-key (kbd "C-x C-b") 'smex)
;;----------------------------------------------------------

;;Projectile----------------------------------------------
(projectile-mode)
;;----------------------------------------------------------

;;misc editing stuff----------------------------------------
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)
(turn-on-font-lock)
(column-number-mode)
(fset 'yes-or-no-p 'y-or-n-p)
;;International keyboards
(require 'iso-transl)
;;----------------------------------------------------------

;;commands enabled------------------------------------------
(put 'narrow-to-region 'disabled nil)
;;----------------------------------------------------------

;;UI-------------------------------------------------------
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)
(when (display-graphic-p)
  (load "font-support.el")
  (set-face-attribute 'default nil :height 110))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-eighties t)
;;----------------------------------------------------------

;;Flycheck--------------------------------------------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;----------------------------------------------------------
