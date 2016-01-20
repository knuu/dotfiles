
;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")

;; hide tool bar, scroll bar
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; mode-line
(display-time-mode 1) ; show time
(line-number-mode 1) ; show line number
(column-number-mode 1) ; show column number

(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (flycheck-mode . "")
    ;; Major modes
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(setq ring-bell-function 'ignore) ; mute bells

;; color-theme
(when (require 'color-theme nil t)
  ;; テーマを読み込むための設定
  (color-theme-initialize))

(color-theme-dark-laptop)

(if (eq window-system 'ns) (progn
;; 英語フォントはMenlo
(set-face-attribute 'default nil :family "Menlo" :height 120)
;; 日本語フォントはヒラギノ明朝
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Osaka"))
;; 半角と全角を1:2に
(setq face-font-rescale-alist '((".Menlo.*" . 1.0) (".*Osaka.*" . 1.2)))
))

;; ハイライト
(defface my-hl-line-face
  ;; 背景darkのときは背景色紺
  '((((class color) (background dark))
     (:background "Navyblue" t))
    ;; 背景がlightなら背景色緑
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; paren-mode
(setq show-paren-delay 0)
(show-paren-mode t)
;; parenのスタイル expressionは括弧内も強調
(setq show-paren-style 'expression)
;; フェイスの変更
;;(set-face-background 'show-paren-match-face nil)
;;(set-face-underline-p 'show-paren-match-face "yellow")

;; セーブファイルをTempディレクトリへ
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" , temporary-file-directory t)))

;; ProofGeneral 4.2
(load-file "~/.emacs.d/elisp/ProofGeneral/generic/proof-site.el")

;; save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'untabify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OCaml
;; append-tuareg.el - Tuareg quick installation: Append this file to .emacs.
(add-to-list 'load-path "~/.emacs.d/elisp/tuareg-2.0.7")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; setting for Japanese character coding
(modify-coding-system-alist 'file "\\.ml\\w?" 'euc-jp-unix)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert
(require 'autoinsert)

;; directory for templates
(setq auto-insert-directory "~/.emacs.d/template/")

;; switch templates by extension
(setq auto-insert-alist
      (nconc '(
	       ("\\.tex$" . ["template.tex" my-template])
	       ("\\.cpp$" . ["template.cpp" my-template])
	       ("\\.py$"  . ["template.py"  my-template])
               ) auto-insert-alist))
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package-manager
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; auto-install
(require 'auto-install)
(auto-install-compatibility-setup)

;; redo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; fly-check(fly-make)
(global-flycheck-mode t)

;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; quickrun
(require 'quickrun)

(quickrun-add-command "c++/c11"
                      '((:command . "g++-5")
                        (:exec    . ("%c -std=c++11 %o -o %e %s"
                                     "%e %a"))
                        (:remove  . ("%e")))
                      :default "c++")

(quickrun-set-default "c++" "c++/c11")

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; python-mode, python-pep8
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell (TODO!!)

;; PATH
(dolist (dir (list
	      "/bin"
	      "usr/bin"
	      "/usr/local/bin"
	      "/Users/admin/.opam/system/bin"
	      "/Users/admin/.rbenv/bin"
	      "/Users/admin/.rbenv/shims"
	      "/usr/local/teTeX/bin"
	      (expand-file-name "~/bin")
	      ))

  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;; MANPATH
(setenv "MANPATH" (concat "/Users/admin/.opam/system/man:/usr/local/man:/usr/share/man" (getenv "MANPATH")))

;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      (error "can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
