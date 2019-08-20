(require 'cl)
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(defvar gpopov/packages '(solarized-theme
			  auto-complete
			  autopair
                          writegood-mode
			  markdown-mode
                          yaml-mode
			  elpy)
  "Default packages")

(defun gpopov/packages-installed-p ()
  (loop for pkg in gpopov/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (gpopov/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg gpopov/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Enable elpy for improved Python mode
(elpy-enable)

;; Set theme for window and non-window mode
(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))

;; Avoid yes/no and use y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Hide the menu at the top of the screen
(menu-bar-mode -1)

;; Show both line and column number
(column-number-mode)

;; Delete selected text when typing
(delete-selection-mode t)

;; Disable tabs
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Disable back-up files
(setq make-backup-files nil)

;; Configure mode for simple string-based auto-complete
(require 'auto-complete-config)

;; Show matching parenthesis pairs
(show-paren-mode t)

;; Close pairs automatically
(require 'autopair)
(electric-pair-mode 1)

;; Highlight trailing whitespaces
(setq-default show-trailing-whitespace t)

;; After saving Python/C files delete the trailing whitespaces
(add-hook 'python-mode-hook
	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'c-mode-common-hook
	  (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Utility functions
(defun untabify-buffer ()
  "Replace tabs with spaces"
  (interactive)
  (untabify (point-min) (point-max)))

(defun clean-whitespaces ()
  "Clean-up the whitespaces in the current buffer"
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun goto-def-or-rgrep ()
  "Go to definition of thing at point or do an rgrep in project if that fails"
  (interactive)
  (condition-case nil (elpy-goto-definition)
    (error (elpy-rgrep-symbol (thing-at-point 'symbol)))))

;; Key binding
(define-key elpy-mode-map (kbd "M-.") 'goto-def-or-rgrep)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c a") 'auto-complete-mode)
(global-set-key (kbd "C-c n") 'clean-whitespaces)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
