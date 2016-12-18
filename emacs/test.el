;; データ構造
;; integer
(type-of 1)
;; float
(type-of 1.0)
;; string
(type-of "1")
;; vector
(type-of [])

;; cons
;; (1 . 2)
(type-of (cons 1 2))
;; cons(as list) ... last element is nil
(type-of (cons 1 (cons 2 nil)))
(cons 1 (cons 2 nil))

;; 破壊的関数（副作用のある関数）
(setq lst1 '(1 2 3))
(nreverse lst1) ;; (3 2 1)
(print lst1)    ;; (1) 破壊的以上の破壊...

;; 非破壊関数（副作用ののない関数）
(setq lst2 '(1 2 3))
(reverse lst2)  ;; '(3 2 1)
(print lst2)    ;; '(1 2 3)

;; スコープ
;;;; スコープには２種類あり、レキシカルスコープでもうひとつはダイナミックスコープ
;;;; Lispは両方を持つ
;;;; レキシカルスコープの例

;;;; 自由変数 ... ある関数に束縛されていない、動的に変化する変数（以下、free）
;;;; 束縛変数 ... ある関数に束縛されている変数
;;;; 関数のどこに位置しているかで、自由と束縛は変換する
;;;; scope-test関数における、freeの扱いは自由変数？
;;;; scope-test関数外における、freeの扱いは束縛変数？
(let ((free 10))
  (defun scope-test (bind) (+ bind free))
  (scope-test 5))

;;; クロージャ ... 関数と変数束縛の一式　または　ローカルな状態を持った関数
(defun upper (lst)
  "this is an example for clojure"
  (mapcar #'(lambda (x) (upcase x)) lst))
(upper '("this" "is" "an" "exampl"))

;;;; 自由変数がグローバルに定義されている場合はグローバルから取得
;;;; 束縛環境が見つかればその環境から取得
(setq y 10)
(let ((z 10))
  (defun scope-test (x) (list x y))
  (scope-test 1)) ;; (1 10)

;; クロージャ =? ローカル状態を持つ関数
(let ((idx 0))
  (defun inc () (setq idx (+ idx 1)))
  (defun dec () (setq idx (- idx 1))))

;1
(inc)
;2
(inc)
;1
(dec)
;0
(dec)


;; 関数のキーワードパラメータ
;; optional
(let
    (
     (opt     (lambda (&optional a) (print a)))
     (key     (lambda (&key k) (print k)))
     (keyopt  (lambda (&optional &key k) (print k)))
     (rest    (lambda (&rest xs) (print xs)))
     )
  (eq nil (funcall opt))
  (funcall opt 1)        ;; 1
  (funcall key :k 1)     ;; 1
  (funcall key :k 2)     ;; 2
  (funcall keyopt :k)    ;; nil
  (funcall rest 1 2 3 4) ;; (1 2 3 4)
  (funcall rest 1 2 3)   ;; (1 2 3)
  )

;; 関数のキーワードパラメータ
;; 応用
;; ref Common Lispと関数型プログラミングの基礎
((lambda (x y &key op)
   (case op
     (:add (+ x y))
     (:mul (* x y)))) 1 2 :op :add)


;; 恒等写像
(defun e (x) x)
(eq 1 (e 1))

;; 合成関数
;; fs .. function list(可変）
(defun mycompose(&rest fs))

;; リスト関数 - last
(last `(1 2 3))
;; リスト関数 - last以外
(butlast `(1 2 3))

;; 高階関数
;; ref http://www.ne.jp/asahi/alpha/kazu/elisp.html
(defun foldr (kons knil ls)
  (if (null ls)
      knil
    (funcall kons (car ls) (foldr kons knil (cdr ls)))))
(foldr 'cons nil '(1 2 3))

;; 合成関数
;; ref http://www.ne.jp/asahi/alpha/kazu/elisp.html
(defun compose (&rest fns)
  (if fns
      (lexical-let ((fns fns))
	(lambda (&rest args)
	  (foldr #'funcall (apply (car (last fns)) args) (butlast fns))))
    #'identity))

;; 相補関数
;; ref http://www.ne.jp/asahi/alpha/kazu/elisp.html
(defun complement (pred)
  (compose #'not pred))


;; 相補関数
;; nil tn
(values (complement t) (complement nil))

;; 逆関数


;; 代入の一般化
(destructuring-bind
    (a b c d) '(0 1 2 3) (list a b c d))

;; 多値の扱い
;; (1 2 3)
(values 1 2 3)

;; 多値を返却する
;; (1 2 3)
((lambda () (values 1 2 3)))

;; 多値の受け取りを一般化する
(defmacro mvbind (&rest args)
    `(multiple-value-bind ,@args))
(multiple-value-bind (a b c) (values 1 2 3) (setq x (+ a b)) (setq y (+ x c)) (print c))
(mvbind (a b c) `(1 2 3))
(multiple-value-bind (a b c) `(1 2 3) (princ a))

;; local関数
(let ((f (lambda (x) (* x x)))) (funcall f 10))

;; 変数を束縛する関数
(let ((x 5)) (* x x))

;; 関数を束縛する関数
(labels ((f1 (x) (+ x x))) (f1 1))

;; values関数（多値関数）
((lambda (x) (values (- x 1) x (+ x 1))) 1)
((lambda (x) (values (- x 1) x (+ x 1))) 10)

;; 相補関数（逆関数？）
;; f = x, f^-1 = f~=1/x f*f~=1

;; リスト操作（や高階関数など）
;; filterっぽい関数
(remove-if #'(lambda (x) (not (numberp x))) '(a 1 2 3 4 f)) ;; (1 2 3 4)
(remove-if #'(lambda (x) (numberp x)) '(a 1 2 3 4 f)) ;; (1 2 3 4)

;; filter関数をremove-if-notでマクロ化する
(defmacro filter (selector lst) `(remove-if-not ,selector ,lst))
;; 数字のものだけフィルタして、数字のものから、２の倍数になるものだけをフィルタする
(filter #'(lambda (x) (= (mod x 2) 0)) (filter  #'(lambda (x) (numberp x)) `(a 1 b 2 c 3 d 4)))
(filter #'(lambda (x) (if (numberp x) (if (= (mod x 2)) t))) `(a 1 b 2 c 3 d 4))

;; 真理値関数
(let
    ((lst '(1 a 2 b 3 c 4 d))
     ;; 数値か？     
     (number (lambda (x) (numberp x)))
     ;; 奇数か？
     (even (lambda (x) (evenp x)))
     ;; 偶数か？
     (odd-1 (lambda (x) (not (evenp x))))
     (odd-2 (lambda (x) (complement (evenp x))))
     ;; リストの最後の要素かどうか
     (islast (lambda (xs) (endp xs)))
     ;; test function add here
     )
  (values
   (cons `("number only lst") (-map number lst))
   (cons "number only and even value only lst" (-map even (filter number lst)))
   (cons "last" (islast `()))
   ))


;; group関数（集合にわける）
;; 末尾再帰を利用する
;; 末尾再帰が無限ループにならないようにする
;; 極力シンプルな関数で実装し
;; 実装時にコメントを残し、そのコメントを元に徐々に末尾再帰化する
;; why末尾再帰? -> compile時に、loopに変換されるため
;; OnLisp p.396より
(defun group (source n)
  (if (endp source)
      nil
    (let ((rest (nthcdr n source)))
      (cons (if (consp rest) (subseq source 0 n) source)
	    (group rest n)))))

(defun iszero (x)
    "alias of zerop"
    (zerop x))

(defun isconscell (xs)
    "alias of consp"
    (consp xs))

(consable `())
(group '(1 2 3 4 5 6) 2)

;; 利用している関数を確認
;; endp リストの最後かを判定する
;; t
(endp `())
;; nil
(endp `(1))

;; nil
(consp 1)
;; t
(consp `(1 2))

;; subseq function
;; list の n th and m thまでを取り出す
;; (1 2 3 4 5)
(subseq `(1nnnnnnnn 2 3 4 5) 0 5)
;;(2 3 4)
(subseq `(1 2 3 4 5) 1 4)

;; nthchar function
;; n+1番目以降をcdrする
(nthcdr 2 `(1 2 3 4 5)) ; (3 4 5)
(nthcdr 5 `(1 2 3 4 5)) ; nil

;; rest function  equal cdr
(rest `(1 2 3))

;; cons function = concat list
;(1 2 3)
(cons 1 `(2 3))
;((1 2) 2 3)
(cons (list 1 2) (list 3 4))

;; シンボルの生成 gensym
(dolist (x `(1 2 3)) (gensym))
(let ((x (gensym))) (print x))

(defmacro fn (expr) '#',(rbuild expor))
(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
    (if (eq (car expr) 'compose)
	(build-compose (cdr expr))
      (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g) (,op ,@(mapcar #'(lambda (f) `(,(rbuild f) ,g)) fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g) ,(labels ((rec (fns) (if fns `(,(rbuild (car fns)) ,(rec (cdr fns))) g))) (rec fns)))))
(macroexpand (fn (and integerp oddp)))
(fn (and integerp oddp))

(values
  (and t t)
  (and t nil)
  (and nil t)
  (and nil nil))

(values
  (and t t)
  (and t nil)
  (and nil t)
  (and nil nil))

;; prefix "-" front of functoin means dash library functions call
;; (1 2 3 4)
(-flatten `(1 2 (3 4) 5 6))
(-flatten `((1 2) (3 4) (5 6)))
(-flatten `((1) (2 3) (4) (5 6)))
;; (1 4 9 16)
(-map (lambda (x) (+ x x)) `(1 2 3 4))

;; not using dash library
(mapcar '1+ `(1 2 3))
(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; my macro
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

