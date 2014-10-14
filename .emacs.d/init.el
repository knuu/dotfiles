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




;; Mac用フォント設定
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

;; 英語
;(set-face-attribute 'default nil
;           :family "Menlo" ;; font
;          :height 100)    ;; font size

;; 日本語
;(set-fontset-font
; nil 'japanese-jisx0208
;; (font-spec :family "Hiragino Mincho Pro")) ;; font
;  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
 
;; 半角と全角の比を1:2に
;(setq face-font-rescale-alist
;      '((".*Hiragino_Mincho_pro.*" . 1.2)))

;(set-background-color "#98bc98") ;; background color
;(set-foreground-color "black")   ;; font color

;; flex-mode
;(add-to-list 'load-path "~/.emacs.d")
;(require 'flex-mode)
;(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))

;; bison-mode
;(add-to-list 'load-path "~/.emacs.d")
;(require 'bison-mode)
;(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))

;; ProofGeneral 4.2
;(load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el")

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
