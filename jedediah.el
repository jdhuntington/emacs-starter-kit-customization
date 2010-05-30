(defun custom-add-exec-path (path)
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":" path))
    (setq exec-path (cons path exec-path))))

(custom-add-exec-path "/Users/jedediah/homebrew/bin")
(custom-add-exec-path "/www/bin")

(defun jlh-duplicate-this-line ()
  "Duplicates the line point is on. With prefix arg, duplicate current line this many times."
  (interactive)
  (save-excursion
    (push-mark)
    (let ((orig-line (line-number-at-pos (beginning-of-line))))
      (copy-region-as-kill (line-beginning-position) (progn (forward-line 1)
                                                            (if (= (line-number-at-pos (beginning-of-line)) orig-line)
                                                                (progn
                                                                  (end-of-line)
                                                                  (newline)))
                                                            (point))))
    (yank)))

(defun indent-whole-buffer ()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun jlh-insert-hashrocket ()
  (interactive) (insert " => "))

(setq default-frame-alist
      '((cursor-type . bar)
        (font . "-apple-monaco-medium-r-normal--15-140-72-72-m-140-iso10646-1")))
(color-theme-blackboard)
(global-auto-revert-mode t)
(server-start)

(require 'smex)
(smex-initialize)
(require 'mode-compile)

(require 'muse-mode)
(setq muse-project-alist
      '(("mywiki" ("~/Dropbox/wiki" :default "index"))))
(setq muse-file-extension nil
      muse-mode-auto-p t)
(add-hook 'find-file-hooks 'muse-mode-maybe)

(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global "ty" 'dabbrev-expand)
(key-chord-define-global "f;" 'jlh-insert-hashrocket)

(global-unset-key (kbd "C-;"))
(global-unset-key (kbd "C-\\"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-x"))

(global-set-key (kbd "C-;") 'jlh-insert-hashrocket)
(global-set-key (kbd "C-\\") 'dabbrev-expand)
(global-set-key (kbd "C-x C-o") 'jlh-duplicate-this-line)
(key-chord-define-global "xo" 'jlh-duplicate-this-line)
(global-set-key (kbd "C-x t") 'indent-whole-buffer)
(global-set-key (kbd "C-z") 'execute-extended-command)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c a") 'ack)

(global-set-key (kbd "<f5>") 'ruby-compilation-this-buffer)
(global-set-key (kbd "<f6>") 'rspec-verify)
(global-set-key (kbd "S-<f6>") 'rspec-verify-single)

;;; scratchish

(add-to-list 'load-path "~/.emacs.d/elpa-to-submit/haskell-mode-2.7.0/")
(require 'haskell-mode)
(require 'inf-haskell)
(autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode" "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-to-list 'auto-mode-alist '("\\.[hg]s$"  . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hi$"     . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[hg]s$" . literate-haskell-mode))

