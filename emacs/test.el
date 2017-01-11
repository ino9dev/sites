;; データ構造
;; ("integer" "float" "symbol" "string" "vector" "cons")
(mapcar (lambda (x) (format "%s" (type-of x))) `(1 1.0 a "1" [] `()))

;; 破壊的関数（副作用のある関数）
(setq lst1 '(1 2 3))
(nreverse lst1) ;; (3 2 1)
(print lst1)    ;; (1) 破壊的以上の破壊...

;; 非破壊関数（副作用のない関数）
(setq lst2 '(1 2 3))
;; '(3 2 1)
(reverse lst2)
;; '(1 2 3)
(print lst2)

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

;; emacsにはクロージャがない
;; Common Lispであれば、以下動作するはず
(setf f
      (let ((i 0))
	(lambda () (incf i))))

(funcall f)

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
;; case文めっちゃ便利
;; ref Common Lispと関数型プログラミングの基礎
;; (3 -1 2)
(let
    (
     (f (lambda (x y &key op)
	  (case op
	    (:min (- x y))
	    (:add (+ x y))
	    (:mul (* x y))))))
  (values (funcall f 1 2 :op :add) (funcall f 1 2 :op :min) (funcall f 1 2 :op :mul)))

((lambda (x y &key op)
   (case op
     (:add (+ x y))
     (:mul (* x y)))) 1 2 :op :mul)


;; 恒等写像
(defun e (x) x)
(eq 1 (e 1))

;; 合成関数
;; fs .. function list(可変）の関数リスト
;; 
;; fn1でfunction listの最初の初期値にまずは適用する
;; その結果に対して、あとは初期値
;;
;; ref "OnLisp" http://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/

;; 関数fとgから、合成関数f.g = f(g(x))を作る
;; 関数をひとつづつ取り出して、適用していく
;; memo まだcompose動作しない
(defun compose(&rest fs)
  (if fs
      (let* ((fn1 (car (last fs)))
	    (fno (butlast fs)))
	#'(lambda (&rest args)
	    (reduce #'funcall fno :from-end t :initial-value (apply fn1 args))))
    #'identity))

;; reduceするのは引数の方

(reduce #'list `(1 2 3 4 5) :initial-value 0)
(reduce #'(lambda (x y) (+ x y)) `(1 2 3 4 5) :initial-value 0)


(funcall #'(lambda (x) x) 1)

(funcall (compose #'+1 #'oddp) 1 2 3)
(funcall (compose #'+1 #'oddp) `(1))

(reduce (lambda (x y) (+ x y)) `(1 2 3))

((let ((fno `(#'+1 #'/ #'oddp))) (lambda (&rest args)
				  (reduce (funcall args :from-end t fno)))

;; not empty list equals t
(let ((l `(1))) (if l 1 2))

;; empty list equals nil
(let ((l `())) (if l 1 2))


;; リスト関数
;; スーパー基本
;; car/cdr .. cons（dotpair）の前ポインタ側と後ポインタ側を取り出す
;; carの方は、要素が一つしかないので、list形式ではなく要素そのものを取り出す
;; 1
(car (cons 1 (cons 2 3)))
;; (2 . 3)
(cdr (cons 1 (cons 2 3)))
;; `1
(car `(1 2 3 4 5))
;; `(2 3 4 5)
(cdr `(1 2 3 4 5))

;;(1 2 3)
(cons 1 `(2 3))
;;((1 2) 3 4)
(cons (list 1 2) (list 3 4))
;;((1 2) (3 4))
(cons (list 1 2) (cons (list 3 4) nil))

;; リストの連想配列?
;; ("b" . 2)
(assoc "b" `(("a" 1) ("b" 2) ("c" 3))))



;; 文字列操作関数
;; ("ho" "ge" "ho" "ge")
(split-string "ho ge ho ge")

;; "1"
(char-to-string 49)

;; "1234"
(int-to-string 1234)

;; ヴェクタ関数


;; 等価関数
;; ref http://eli.thegreenplace.net/2004/08/08/equality-in-lisp
;; 良くない例
;; 処理系によってはtになり，処理系によってはtにならない
(eq 1 1)
(eq 'a 'a)

;; 同じクラスの同一の数字、同一の文字であることを判定するには、eqlを使うことになっている。
;; ref http://g000001.cddddr.org/1198855317
;; t
(eql 1 1)
;; nil
(eql 1 1.0)
;; t
(eql 'a 'a)
;; listの同一性にはequalを使う
;; nil
(eq `(1 2) `(1 2))
;; t
(equal `(1 2) `(1 2))

;; function的同型はどうだろう？
;; t
(equal (lambda (x) ()) (lambda (x) ()))
;; nil
(equal (lambda (x) ()) (lambda (x) (x))) ;; すげぇ!?本当かな!?

;; subseq function
;; list の from n to length mまでを取り出す

;; nil
;;   idx->0 1 2 3 4
;;   len->1 2 3 4 5
(subseq `(1 2 3 4 5) 0 0)

;; (1)
;;   idx->0 1 2 3 4
;;   len->1 2 3 4 5
(subseq `(1 2 3 4 5) 0 1)

;; (1 2 3)
;;   idx->0 1 2 3 4
;;   len->1 2 3 4 5
(subseq `(1 2 3 4 5) 0 3)

;; (1 2 3 4 5)
;;   idx->0 1 2 3 4
;;   len->1 2 3 4 5
(subseq `(1 2 3 4 5) 0 5)

;;(2 3)
;;   idx->0 1 2 3 4
;;   len->1 2 3 4 5
(subseq `(1 2 3 4 5) 1 3)

;;(2 3 4)
;;   idx->0 1 2 3 4
;;   len->1 2 3 4 5
(subseq `(1 2 3 4 5) 1 4)

;; n+1番目以降をcdrする
;; (3 4 5)
(nthcdr 2 `(1 2 3 4 5))

;; nil
(nthcdr 5 `(1 2 3 4 5))

;; rest function equal cdr
;; `(2 3)
(rest `(1 2 3))
;; t
(equal (cdr `(1 2 3)) (rest `(1 2 3)))



;;(1 2 3 4)
(append `(1 2) `(3 4))

;; 末尾再帰などで出てくるのが以下のようなcar/cdrの使い方にその深遠さを感じる
;; 末尾再帰 ... 
(defun fact (xs)
  (if (eq xs nil)
      1
    (* (car xs) (fact (cdr xs)))))

;; 120
(fact `(1 2 3 4 5))
;; 以下と同じ
(* (* (* (* (* 1 1) 2) 3) 4) 5)

;; 空リストにcar/cdrをかけるとnil
; nil
(cdr `())
; nil
(car `())

;; というか、結果的には以下と同じだよね...
(* 1 2 3 4 5)

;; last関数 .. 最後の要素をリストで返す
;; `(3)
(last `(1 2 3))
;; butlast関数 .. 最後の要素以外をリストで返す
;; `(1 2)
(butlast `(1 2 3))
;; nth .. n番目の要素を返す,0番目は1個目
;; 2
(nth 1 `(1 2 3))

;; 論理演算(論理関数?)
;; (t t t nil)
(let ((test `((t t) (t nil) (nil t) (nil nil))))
  (mapcar (lambda (x) (or (nth 0 x) (nth 1 x))) test))
;; (t nil nil nil)
(let ((test `((t t) (t nil) (nil t) (nil nil))))
  (mapcar (lambda (x) (and (nth 0 x) (nth 1 x))) test))

;; 

;; [note]
;;
;; andのfunction descriptionにはこう書いてある
;; いわゆるjavaとかのandと同じ振る舞いをする
;; tのところまでは評価されnil(false)が発生したところで、評価を停止する
;; Eval args until one of them yields nil, then return nil.
;; 評価対象のいくつかの引数のうち一つがnilを生じるならば、(and関数は）nilを返す
;; The remaining args are not evalled at all.
;; 残った引数は全く評価されない
;; If no arg yields nil, return the last arg's value.Eval args until one of them yields nil,
;; then return nil.
;; The remaining args are not evalled at all.
;; 残った引数は全く評価されない
;; If no arg yields nil, return the last arg's value.
;; もし、引数がnilを生じないなら、最後の引数の値を返す

;; テストしてみる
;; テスト対象は4で偶数であり、4で割り切れる数
;; 2の場合 -> 最初のevenpはt で mod4pもtの場合は、mod4p側もt
;; t
((lambda (x)
   (flet ((mod4p (x) (if (eq 0 (mod x 4)) t nil)))
     (and (evenp x) (mod4p x)))) 4)
;; 2の場合 -> 最初のevenpはt で mod4pはnilの場合は、mod4p側のnilが返却されていると考えられる
;; nil
((lambda (x)
   (flet (
	  (mod4p (x) (if (eq 0 (mod x 4)) t nil))
	  (eq4 (x) (if (eq 4 x) t nil)))
     (and (evenp x) (mod4p x) (eq4 x)))) 8)

;; 分岐関数
;; (t nil) -> (nil t)
;; 数学的には場合分け
(mapcar (lambda (x) (if (eq x t) nil t)) `(t nil))

;; 分岐マクロ
;; 1 -> one
;; 2-> two
;; 2-> three
(mapcar (lambda (x) (case x (1 'one) (2 'two) (3 'three))) `(1 2 3))

;; 高階関数
;; ref http://www.ne.jp/asahi/alpha/kazu/elisp.html
(defun foldr (kons knil ls)
  (if (null ls)
      knil
    (funcall kons (car ls) (foldr kons knil (cdr ls)))))

(foldr #'cons nil '(1 (2 3) 4 (5 (6 7))))

(reduce #'list `(1 2 3 4 5) :from-end t)
(reduce #'list `(1 2 3 4 5) :from-end nil)

(mapcan #'(lambda (x) (and (numberp x) (list x))) '(a 1 b c 3 4 d 5))
(mapcar #'(lambda (x) (and (numberp x) (list x))) '(a 1 b c 3 4 d 5))

(funcall (lambda (x) (and (numberp x) (evenp x))) 2)


(and (numberp 1) (list 1))


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
(defmacro mbind (vars args fn)
  `(multiple-value-bind ,vars ,args ,fn))
(mbind (a b c) `(1 2 3) (format "%d,%d,and %d" a b c))

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
(remove-if #'(lambda (x) (numberp x)) '(a 1 2 3 4 f)) ;; (a f)
;; 現在以下の関数は非推奨となっている
(remove-if-not #'(lambda (x) (numberp x)) '(a 1 2 3 4 f)) ;; (1 2 3 4)

;; filter関数をremove-if-notでマクロ化する
(defmacro filter (selector lst) `(remove-if-not ,selector ,lst))
;; 数字のものだけフィルタして、数字のものから、２の倍数になるものだけをフィルタする
(filter #'(lambda (x) (= (mod x 2) 0)) (filter  #'(lambda (x) (numberp x)) `(a 1 b 2 c 3 d 4)))
(filter #'(lambda (x) (if (numberp x) (if (= (mod x 2)) t))) `(a 1 b 2 c 3 d 4))

;; 述語関数
;; consかどうか
;; (nil t)
(mapcar (lambda (x) (consp x)) `(1 `(2 3)))

;; listの最後かどうか
;; (nil t)
(mapcar (lambda (l) (endp l)) `((1) ()))

;; 数値化、奇数か、偶数か
;; (t nil t nil)
(mapcar (lambda (x) (numberp x)) `(1 a 3 b))
;; (nil t nil t)
(mapcar (lambda (x) (evenp x)) `(1 2 3 4))

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

;; ((1 2) (3 4) (5 6)
(group '(1 2 3 4 5 6) 2)


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
;; test-simple.el
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'test-simple)
(defun E (x) x)
(assert-equal (E 1) 1)
(assert-equal (E t) t)
(assert-t (and (E 1) 1))
(assert-nil (and (not (E 1)) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; my func or macros
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun last-a (x)
  "return last of list as atom"
  (car (last x)))

(defun iszero (x)
    "alias of zerop"
    (zerop x))

(defun isconscell (xs)
    "alias of consp"
    (consp xs))

;; (1  -1  1)
;; (a1 a2 a3)

(let
    ((clist `(1 -1 -1))
     (blist `(a1 a2 a3)))
  (mapcar (lambda (x) (* (car x) (cdr x))) 

(defmacro fomula (&rest clst &rest blst)
  (cons (cons -1 (* x x)) (cons 1 (* x)) (cons 0 (expt x 0))))

(lambda (x y z) (expt x 2))

(setq lst (cons `(1 2) `(3 4)))

(dolist (x (list `(1 2) `(2 3)) sum) (setq z (* (nth 0 x) (nth 1 x))))

(let ((x 0) (y 0) (z 0)) (mapcar (lambda (xs) (* (nth 0 xs) (nth 1 xs))) (list `(1 x) `(3 y) `(5 z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; applied or experimental mathematics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 微分する
;; 引数に関数と、何で微分するかを渡す
;; case1) １変数の場合
;;   target x
;;   (defun f (x) (* x x)) -> (lambda (x) ((f(+ x 0.1) - (f x))/0.1)
;; case2) 多変数の場合
;;   target x
;;   (defun f (x y) (+ x y)) -> (lambda (x) (((f
;; case3) 三角関数の場合
;;   target x
;;   (defun f (x) (sin x))
;; 指定した変数での微分結果としての関数が返却させる
(defun diff (fbody target)
  "return diffed function"
  (eval
   (car (read-from-string
     (concat
      "("
      (format "lambda (%s) " target)
      "(/ "
      "(- "
      (replace-regexp-in-string
       (format "%s" target)
       (format "(+ %s 0.000001)" target)
       (format "%s" fbody))
      " "
      (format "%s" fbody)
      ")"
      " " 
      "0.000001)"
      ")"
      ")"
      )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 関数を与えた際に、その微分関数を返す
;;
;; 引数
;;   fn関数そのものを渡せるようにしたバージョン（無名関数）
;; また、多変数について微分可能にする
;; f(x)を与えた場合、再帰関数のようにd = 微小な差分 f(x + d) - f(x) / dをとってfを即時実行しないで返す
(defun diff2(fn target)
  "return diffrencial function of FN
FN is a function, TARGET is a list of differncial target."
  (eval
   (let* (
	  (delta 0.0000001)
	  (ttarget target)
	(dtarget (mapcar (lambda (e) `(+ ,e ,delta)) ttarget)))
    `(lambda (,@ttarget) (/ (- (,fn ,@dtarget) (,fn ,@ttarget)) ,delta)))))

;; x^2/dx≒2 x
;; 2.0000...
(funcall (diff2 (lambda (x) (* x x)) `(x)) 1)
;; x^2+2x+1/dx≒2x+2
;; 4.0000...
(funcall (diff2 (lambda (x) (+ (* x x) (* 2 x) 1)) `(x)) 1)
;; 6.0000...
(funcall (diff2 (lambda (x) (+ (* x x) (* 2 x) 1)) `(x)) 2)
;; d(sinx)/dx ≒ -cosx
;; memo 誤差が大きいので動作がかなり怪しい、要テスト
(- (cos (/ pi 2)))
(funcall (diff2 (lambda (x) (sin x)) `(x)) (/ pi 2))
;; d(cosx)/dx ≒sinsx
(- (sin (/ pi 2)))
(funcall (diff2 (lambda (x) (cos x)) `(x)) (/ pi 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gen-snipet-select-oneline
;; 指定した言語で、あるファイルから１行だけフィルタするsnipetを生成する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gen-snipet-select-oneline (lang)
  (interactive ("filename" filename))
  ;; 引数を得る
  (let ((snippet ""))
    (case lang
      (bash
       (setq snippet (format
	"cat <<filename>> | head <<targetnum-1>> | tail -1")))
      (powershell
       (format
	"Get-Content <<filename>> | select -skiip <<targetnum-1>> -first 1"))
      (java
       (format
	"File file = new FileReader(\"<<filename>>\");\nint linecount = 0;\nwhile(!line = file.readline()){\n\tif(<<targetcount>>.equals(<<linecount>>)){\n\t\tSystem.out.println(line);}\n\t\tlinecount=linecount+1;\n\t}\n}"))
      )
    (insert "\n" snippet))
  )

(gen-snipet-select-oneline 'bash)
(gen-snipet-select-oneline 'powershell)
(gen-snipet-select-oneline 'java)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gen-snipet-foreach-if
;; 指定した言語で、繰り返し処理をするsnipetを生成する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gen-snipet-find-by-keyword
;; 指定した言語で、あるファイルからある言葉を含むファイルを検索する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



