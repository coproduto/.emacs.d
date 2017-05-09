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
(package-initialize)
;;----------------------------------------------------------

;;Make sure we get the correct environment variables---------
(exec-path-from-shell-initialize)
;;-----------------------------------------------------------

;;JS mode---------------------------------------------------
(require 'js)
(setq js-indent-level 2)
;;----------------------------------------------------------


;;Python mode-----------------------------------------------
(require 'python)
(setq-default python-indent-offset 2)
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
(ido-mode 1)
;;----------------------------------------------------------

;;misc editing stuff----------------------------------------
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)
(turn-on-font-lock)
(column-number-mode)
;;International keyboards
(require 'iso-transl)
;;----------------------------------------------------------

;;commands enabled------------------------------------------
(put 'narrow-to-region 'disabled nil)
;;----------------------------------------------------------

;;GUI-------------------------------------------------------
(add-to-list 'default-frame-alist '(foreground-color . "#D7CEE0"))
(add-to-list 'default-frame-alist '(background-color . "#090909"))
(when (display-graphic-p)
  (load "font-support.el")
  (set-face-attribute 'default nil :height 110))
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
;;----------------------------------------------------------
