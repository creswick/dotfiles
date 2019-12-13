;; ------------------------------------------------------------
;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(setq load-path (cons "~/dotfiles/emacs/lisp" load-path))

;; Put auto-save files in your home directory.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(require 'subr-x)

;; Speed startup
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)

(defvar erc/packages
  '( flymake-hlint
;     magit
     clojure-mode
     go-mode
     haskell-mode
     json-mode
     js2-mode
     jsx-mode
     web-mode
     markdown-mode
     lua-mode
     rust-mode
     gitignore-mode
     gitconfig-mode
     yaml-mode

     ;; Other modes
     company
     ;; Navigation
     projectile
     helm
     helm-projectile
     helm-company
     helm-ag
     ag
     dumb-jump
     transpose-frame
     ace-window
;     avy
     ;; Writing
     typo
     auctex
     ;; Other
     smart-mode-line
     undo-tree
     edit-server
     ;; nix
     nix-mode)
  "A list of packages to ensure are installed at launch.")

(require 'cl)
(require 'uniquify)

;; Line numbers:
(setq linum-format "%3d\u2502")

(defun erc/packages-installed-p ()
  (loop for pkg in erc/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (erc/packages-installed-p)
  (message "%s" "Refreshing package database")
  (package-refresh-contents)
  (dolist (pkg erc/packages)
    (when (not (package-installed-p pkg))
            (package-install pkg))))

;; ace-window:
(global-set-key (kbd "M-o") 'ace-window)

;; Disable the useless menu bar
(menu-bar-mode -1)

;; Disable the tool bar.
(tool-bar-mode nil)

;; More expressive undo
(global-undo-tree-mode)

;; Handy util to delete things
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      ;; (if (vc-backend filename)
      ;;     (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))) ;)

;; ------------------------------------------------------------
;; Haskell

'(haskell-check-command "hlint")
'(haskell-hoogle-command nil)
'(haskell-stylish-on-save t)
'(haskell-tags-on-save t)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(require 'flymake-hlint)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'flymake-hlint-load)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-align-imports-pad-after-name t)
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-import-mapping nil)
 '(haskell-mode-hook
   (quote
    (hlint-refactor-mode projectile-mode flymake-hlint-load turn-on-haskell-doc-mode haskell-indentation-mode)))
 '(haskell-process-path-ghci "/home/rogan/Groq/Haskell/runGhci.sh")
 '(haskell-process-prompt-restart-on-cabal-change nil)
 '(haskell-process-type (quote ghci))
 '(package-selected-packages
   (quote
    (avy nix-mode json-mode yaml-mode web-mode use-package undo-tree typo transpose-frame smart-mode-line rust-mode markdown-mode lua-mode jsx-mode js2-mode htmlize helm-projectile helm-company helm-ag go-mode gitignore-mode gitconfig-mode flymake-hlint edit-server dumb-jump clojure-mode auctex ag))))

;; (with-eval-after-load 'haskell-mode-hook
;;   (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports))

;; compilation:
(defun compile-project ()
  (interactive)
  (let ((default-directory
          (if (or (string= (file-name-extension buffer-file-name) "hs")
                  (string= (file-name-extension buffer-file-name) "cabal"))
              ;; then find a stack.yaml:
              (locate-dominating-file buffer-file-name "stack.yaml")
            default-directory))))
  (call-interactively #'compile))

;; recompile:
(global-set-key (kbd "<f12>") 'recompile)
(global-set-key (kbd "\C-c b") 'recompile)
(global-set-key (kbd "\C-c \C-c") 'haskell-compile)

(global-set-key (kbd "<f8>") 'haskell-navigate-imports)

;; hlint refactor keybindings:
;; C-c , r - Apply the suggestion under the cursor
;; C-c , b - Apply all suggestions in the buffer
(require 'hlint-refactor)
(add-hook 'haskell-mode-hook 'hlint-refactor-mode)

(use-package projectile
   :ensure t
   :bind-keymap (("C-c p" . projectile-command-map)))

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'projectile-mode-hook
          (lambda ()
            ;; Identify a Haskell project root by the location of its "Setup.hs" file
            (add-to-list 'projectile-project-root-files "Setup.hs")
            (setq projectile-test-suffix-function
                  (lambda (project-type)
                    (if (member project-type '(haskell-cabal haskell-stack))
                        "Test"
                      ;; call the original implementation if it's not a haskell project
                      (projectile-test-suffix project-type))))))

(add-hook 'haskell-mode-hook 'projectile-mode)

;; Configure avy
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

;; (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
;; Disable electric-indent-mode, which is enabled by default in Emacs
;; 24.4, and is very annoying haskell-mode (e.g. it attempts to indent
;; every top-level definition.
;; (add-hook 'haskell-mode-hook '(lambda () (electric-indent-mode 0)))

;; Unicode symbols
;; (defvar haskell-font-lock-symbols)
;; (setq haskell-font-lock-symbols t)

;; (require 'haskell-unicode-input-method)
;; (add-hook 'haskell-mode-hook
;;   (lambda () (set-input-method "haskell-unicode")))

;; ------------------------------------------------------------
;; SAL

;; (autoload 'sal-mode "sal" "Major mode for SAL." t)
;; (autoload 'run-sal "sal" "Switch to interactive SAL buffer." t)
;; (setq auto-mode-alist (cons '("\\.sal" . sal-mode) auto-mode-alist))

;; ------------------------------------------------------------
;; Latex

;; Turns on reftex mode when running latex mode.  Check out reftex and
;; all of its wonderful features!!
(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq auto-mode-alist (cons '("\\.tex" . latex-mode) auto-mode-alist))

;; Load Jex files with json-mode:
(setq auto-mode-alist (cons '("\\.jex" . json-mode) auto-mode-alist))

;; Load alan-assembly files with asm-mode:
(require 'alan-assembly-mode)
(setq auto-mode-alist (cons '("\\.aa" . alan-assembly-mode) auto-mode-alist))

;; ------------------------------------------------------------
;; Magit

;(setq magit-last-seen-setup-instructions "1.4.0")
;(global-set-key (kbd "\C-xm") 'magit-status)


;; Set the smerge prefix to C-c m (was C-c ^)
;;
;;   `C-c m m` <-- keep "my" change.
;;   `C-c m n` <-- next change.
;;   `C-c m <ret>` <-- keep version cursor is on.
(setq smerge-command-prefix "\C-cm")

;; ------------------------------------------------------------
;; Highlight things
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h h") 'highlight-symbol-at-point)
(defun unhighlight-all ()
  (interactive)
  (unhighlight-regexp t))
(global-set-key (kbd "M-h u") 'unhighlight-all)

;; ------------------------------------------------------------
;; Markdown

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; ------------------------------------------------------------
;; Backups

;; No.
(setq make-backup-files nil)

;; ------------------------------------------------------------
;; Text modes

(add-hook 'text-mode-hook 'turn-spell-checking-on)
(add-hook 'text-mode-hook 'visual-line-mode)

;; ------------------------------------------------------------
;; Global settings

;; Show columns
(setq column-number-mode t)

;; Tabs are spaces
(setq-default indent-tabs-mode nil)

;; Show only trailing whitespace
(setq-default show-trailing-whitespace t)

;; show matching parens:
(show-paren-mode t)

;; Turns on the visible bell--this makes the screen flash when you've been
;; naughty or when you have an appointment.
(setq visible-bell t)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Sets the default major mode that emacs starts up in.
(setq default-major-mode 'text-mode)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)



;; ------------------------------------------------------------
;; Global definitions

;; Unfill functions (for "MS Word"-ing text)
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Flyspell
(defun turn-spell-checking-on ()
  "Turn speck-mode or flyspell-mode on."
  (flyspell-mode 1)
  )

;; ------------------------------------------------------------
;; Global key bindings

;; Run align-regexp (in any mode)
(global-set-key (kbd "C-c C-g") 'align-regexp)

;; unfill-paragraph locally defined
(global-set-key (kbd "C-M-q") 'unfill-paragraph)

;; For commenting and uncommenting quickly
(global-set-key (kbd "\C-xu") 'uncomment-region)
(global-set-key (kbd "\C-xc") 'comment-region)

(global-set-key (kbd "\C-xri") 'string-insert-rectangle)

;; -------------------------------------
;; Helm stuff
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(setq helm-split-window-in-side-p t
      helm-ff-skip-boring-files t
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-c h o") 'helm-occur)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(helm-autoresize-mode t)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; Let me use just y or n to answer prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; ----------------------------------------
;; Ivy

;; (ivy-mode 1)

;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")

;; ----------------------------------------
;; Dumb jump

(dumb-jump-mode)
(global-set-key (kbd "M-g j") 'dumb-jump-go)
(global-set-key (kbd "M-g b") 'dumb-jump-back)
;; (setq dumb-jump-selector 'helm)

;; (use-package 'dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g i" . dumb-jump-go-prompt)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config (setq dumb-jump-selector 'helm)
;;   :ensure)


(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-M-d") 'backward-kill-word)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
