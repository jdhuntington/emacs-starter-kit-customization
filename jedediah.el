(defun custom-add-exec-path (path)
  (progn
    (setenv "PATH" (concat path ":" (getenv "PATH")))
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

(defun jlh-join-lines (arg)
  "Join this line to the line above n times
Running this command with an argument of 1 is equivalent
to running 'delete-indentation (aka 'join-line)."
  (interactive "NHow many lines to join?: ")
  (while (> arg 0)
    (join-line)
    (setq arg (- arg 1))))

(defun jlh-curly-wrap ()
  (interactive)
  (save-excursion
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (insert "{")
    (exchange-point-and-mark)
    (insert "}")))

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(defun indent-whole-buffer ()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun jlh-insert-hashrocket ()
  (interactive) (insert " => "))

(defun jlh-sort-buffer ()
  "Sorts While Buffer"
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (sort-lines)))


(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(fset 'jlh-recenter-top
      "\C-u0\C-l")

(fset 'jlh-next-section
      [?\C-x ?n ?w ?\C-s ?\; ?\; ?  ?- ?- ?- ?- ?\C-s ?\C-a ?\C-  ?\C-r ?\C-r ?\C-e ?\C-x ?n ?n ?\C- ])



(setq default-frame-alist
      '((cursor-type . bar)
        (font . "-apple-Droid_Sans_Mono-medium-normal-normal-*-15-*-*-*-m-0-fontset-auto3")))
(color-theme-blackboard)
(global-auto-revert-mode t)
(server-start)

;; (add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)

(require 'mode-compile)
(require 'smex)
(smex-initialize)

(require 'muse-mode)
(setq muse-project-alist
      '(("mywiki" ("~/Dropbox/wiki" :default "index"))))
(setq muse-file-extension nil
      muse-mode-auto-p t)
(add-hook 'find-file-hooks 'muse-mode-maybe)

(require 'key-chord)
(key-chord-mode 1)

(display-time-mode 1)

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
(global-set-key (kbd "S-<f5>") 'other-frame)
(global-set-key (kbd "<f6>") 'rspec-verify)
(global-set-key (kbd "S-<f6>") 'rspec-verify-single)
(global-set-key (kbd "<f7>") 'jlh-recenter-top)
(global-set-key (kbd "S-<f7>") 'jlh-next-section)
(global-set-key (kbd "C-c ^") 'jlh-join-lines)


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


(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
                               auto-mode-alist))

(add-to-list 'load-path "~/.emacs.d/elpa-to-submit/rspec-mode/")
(require 'rspec-mode)

;; from http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
(require 'recentf)
 
(recentf-mode t)
(setq recentf-max-saved-items 500)
 
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

 
;; get rid of `find-file-read-only' and replace it with something
;; more useful.

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)


;; (add-to-list 'load-path "~/.emacs.d/elpa-to-submit/anything-config/")
;; (require 'anything-startup)

(add-hook 'html-mode-hook 'run-coding-hook)
(add-hook 'ack-mode-hook 'run-coding-hook)

(put 'narrow-to-region 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'downcase-region 'disabled nil)
