
(require 'cl)
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))
(defvar emacs-packages '(ac-slime
			 auto-complete
			 autopair
			 clojure-mode
			 feature-mode
			 gist
			 go-mode
			 markdown-mode
			 marmalade
			 nodejs-repl
			 org
			 paredit
			 php-mode
			 restclient
			 rvm
			 scala-mode
			 web-mode
			 yaml-mode)
  "Default packages")

(defun emacs-packages-installed-p ()
  (loop for pkg in emacs-packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (emacs-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg emacs-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 140
                      :weight 'normal
                      :width 'normal)

  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "DejaVu Sans Mono"
                                 :width 'normal
                                 :size 12.4
                                 :weight 'normal))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; This would disable backup files.
;; (setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)


(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(require 'autopair)
(require 'auto-complete-config)

(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))

;; Uses spaces instead of tabs.
(setq tab-width 2
      indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default python-indent-offset 4)
(setq js-indent-level 2)

;; Converting tabs to spaces and removing trailing whitespaces.
(defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

(defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer."
    (interactive)
    (untabify-buffer)
    (delete-trailing-whitespace))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c a") 'auto-complete-mode)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)

(ac-config-default)
(setq-default show-trailing-whitespace t)
(add-hook 'c-mode-common-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


(electric-pair-mode 1)

