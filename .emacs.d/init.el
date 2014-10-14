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

;; ツールバー、スクロールバー非表示
(when window-system
  ;; tool-barを非表示
  (tool-bar-mode 0)
  ;; scroll-barを非表示
  (scroll-bar-mode 0))

;; color-theme
(when (require 'color-theme nil t)
  ;; テーマを読み込むための設定
  (color-theme-initialize))

;; 英語フォントはMenlo
(set-face-attribute 'default nil
		    :family "Menlo"
		    :height 120)

;; 日本語フォントはヒラギノ明朝
(set-fontset-font 
 nil 'japanese-jisx0208
 (font-spec :family "Osaka"))

;; 半角と全角を1:2に
(setq face-font-rescale-alist
      '((".Menlo.*" . 1.0)
	(".*Osaka.*" . 1.2)))

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

;; paren-mode : 括弧の対応
(setq show-paren-delay 0)
(show-paren-mode t)
;; parenのスタイル expressionは括弧内も強調
(setq show-paren-style 'expression)
;; フェイスの変更
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; セーブファイルをTempディレクトリへ
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" , temporary-file-directory t)))

;; auto-install
(require 'auto-install)


;; flex-mode
(require 'flex-mode)
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))

;; bison-mode
(require 'bison-mode)
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))

;; ProofGeneral 4.2
(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")

;; ;;;;
;; bison-mode / flex-mode
;; ;;;;
;(autoload 'bison-mode "bison-mode")
;; *.y *.yy ファイルを 自動的に bison-mode にする
;(setq auto-mode-alist
;     (cons '("\.\(y\|yy\)$" . bison-mode) auto-mode-alist))
;(autoload 'flex-mode "flex-mode")
;; *.l *.ll ファイルを 自動的に flex-mode にする
;(setq auto-mode-alist
;     (cons '("\.\(l\|ll\)$" . flex-mode) auto-mode-alist))
