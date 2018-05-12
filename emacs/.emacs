;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; ino9oni's .emacs
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージシステムを利用する
;;; ref http://emacs.rubikitch.com/test-simple/
(package-initialize)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/")
    ("org"   . "http://orgmode.org/elpa/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; イニシャルメッセージ系を非表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message "")
(setq initial-scratch-message "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windowのセッティング
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ウィンドウ操作をCtrl+x+nで次タブへ移動
;;(global-set-key (kbd "C-x n") (lambda () (interactive) (other-window 1)))
;; from https://qiita.com/saku/items/6ef40a0bbaadb2cffbce
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
;    (split-window-horizontally) ;split horizontally
    (split-window-vertically) ;split vertically
  )
  (other-window val))
;;(global-set-key [C-tab] (lambda () (interactive) (progn (message "hoge") (other-window    1))))
;;(global-set-key (kbd "<C-S-tab>") (lambda () (interactive) (other-window -1)))
;; ウィンドウ操作をCtrl+xt+pで前タブへ移動
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x n") (lambda () (interactive) (other-window  1)))

;; not to display menubar
(menu-bar-mode -1)

(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 自動インストールパッケージを入れておく
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-pakage-name t)
  (auto-install-compatibility-setup))

;;; 必要なライブラリの呼び出し
(require 'cl)

;; theme setting
(load-theme 'wombat)

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)
;; C-hを１文字削除操作に割当て
(global-set-key "\C-h" 'delete-backward-char)
;; C-xC-hを関数
(global-set-key "\C-x\C-h" 'describe-function)

;; default encodingはutf8
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8-with-signature)
(setq ruby-insert-encoding-magic-comment nil)

(global-set-key "\C-c\C-r" 'window-resizer)

;; japanese input method mozc
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;; not to display toolbar


;; not to display scrollbar
;;(set-scroll-bar-mode nil)

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My IDE mode （ソリューションエクスプローラー風）major mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-ide-mode ()
  "私のIDE mode"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "MyIDE")
  (setq major-mode 'my-ide-mode)
  ;; ウィンドウを５個に分割する
  ;; ファイラを左側に dire-toggle(fixed frame keeping display)
  ;; 右側上に、メイン開発用枠（アプリコードとか）
  ;; 右側下左に、サブ開発用枠（デザインとか）
  ;; 右側下右上に、コンパイルやデプロイ用シェル
  ;; 右側下左下に、検証用サービス起動実行用シェル
  (progn
    ;;　ファイラを起動しておいて、ソリューションエクスプローラー風にしておく
    (dired-toggle)

    ;; dired-toggleのウィンドウを少し広げておく
    ;; todo もうすこしモニタの解像度を意識したコードにする    
    (enlarge-window-horizontally 40)

    ;; メイン画面へ移動
    (other-window 1)
    
    ;; 行番号表示しておく
    (split-window-vertically)
    ;; not to display initial message
    (linum-mode t)

    ;; メイン１とメイン２のバッファにわける
    (split-window-horizontally)
    ;; メイン開発用バッファ設定（メイン１）
    (switch-to-buffer (get-buffer-create "*scratch*"))    
    (rename-buffer "*dev-main1*")
    ;; メイン１のウィンドウ幅を少し広げておく
    ;; todo もうすこしモニタの解像度を意識したコードにする
    (enlarge-window-horizontally 30)
    ;; メイン１のウィンドウ高を少し広げておく
    (enlarge-window 10)
    
    ;; 次の画面で作業
    (other-window 1)

    ;; メイン２枚目開発用バッファ設定（メイン２）
    (switch-to-buffer (get-buffer-create "*scratch*"))    
    (rename-buffer "*dev-main2*")
    ;; メイン２のウィンドウ幅を少し広げておく
    ;; todo もうすこしモニタの解像度を意識したコードにする
    (enlarge-window-horizontally 30)    

    ;;サブの画面へ移動
    (other-window 1)

    ;; サブ側設定    
    (split-window-horizontally)
    ;; ウィンドウサイズを変更しておく
    ;; サブ開発用バッファ設定
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (rename-buffer "*dev-sub*")

    ;; シェルバッファへ移動
    (other-window 1)
    
    ;; シェルｘ２立ち上げておく
    (split-window-vertically)
    (eshell '1)
    (other-window 1)
    (eshell '2)
    (other-window 1)
    )
  (run-hooks 'my-ide-hook))

;; memo add-hookとどう違うのか？
(provide 'my-ide-mode)

;; memo
;; (add-hook 'name 'func) -> M-x name で func を callする、funcはdefnされたもの
