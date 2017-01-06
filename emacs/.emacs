;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; ino9oni's .emacs
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; パッケージシステムを利用する
;;; ref http://emacs.rubikitch.com/test-simple/
(package-initialize)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org"   . "http://orgmode.org/elpa/")))

;;; 必要なライブラリの呼び出し
(require 'cl)
(require 'dash)

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

;; Window Resizer
;; ref http://d.hatena.ne.jp/mooz/20100119/p1
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
	(current-width (window-width))
	(current-height (window-height))
	(dx (if (= (nth 0 (window-edges)) 0) 1
	      -1))
	(dy (if (= (nth 1 (window-edges)) 0) 1
	      -1))
	action c)
    (catch 'end-flag
      (while t
	(setq action
	      (read-key-sequence-vector (format "size[%dx%d]"
						(window-width)
						(window-height))))
	(setq c (aref action 0))
	(cond ((= c ?l)
	       (enlarge-window-horizontally dx))
	      ((= c ?h)
	       (shrink-window-horizontally dx))
	      ((= c ?j)
	       (enlarge-window dy))
	      ((= c ?k)
	       (shrink-window dy))
	      (setq auto-insert-alist
		    (append '(
			      ("\\.txt" . "text-insert.txt")
			      ) auto-insert-alist))              ;; otherwise
	      (t
	       (let ((last-command-char (aref action 0))
		     (command (key-binding action)))
		 (when command
		   (call-interactively command)))
	       (message "Quit")
	       (throw 'end-flag t)))))))

(global-set-key "\C-c\C-r" 'window-resizer)

;; japanese input method mozc
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;; autoinsert (for template)
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "lisp/insert/")
(setq auto-insert-alist
      (append '((".*_minutes\\.txt" . "minutes-insert.txt")) auto-insert-alist))

;; 空白文字を可視化する
;; ref ttp://weboo-returns.com/blog/emacs-shows-double-space-and-tab/
(setq whitespace-style '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '(
	(tab-mark   ?\t   [?\xBB ?\t])
       ))

(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")
