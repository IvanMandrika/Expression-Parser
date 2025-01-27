(load-file "libs/proto.clj")
(load-file "libs/parser.clj")
(declare recursive-parser)

;--------------------------------------calculator
(defn div-n
      ([x] (/ 1.0 (double x)))
      ([x y] (/ (double x) (double y)))
      ([x y & more] (reduce div-n (div-n x y) more)))

(defn multi-diff [diffed-args args add-container multiply-container]
      (apply add-container (map (fn [num]
                                    (apply multiply-container (update (vec args) num (fn [_] (nth diffed-args num)))))
                                (range (count args)))))

(defn arMean [& args] (/ (apply + args) (count args)))
(defn gMean [& args] (Math/pow (abs (apply * args)) (/ 1 (count args))))
(defn hMean [& args] (div-n (count args) (apply + (map #(div-n 1 %) args))))

;--------------------------------------functional expression

(defn evaluator [func]
      (fn [& args]
          (fn [vars]
              (apply func (map #(% vars) args)))))

(def constant constantly)

(defn variable [string]
      (fn [vars]
          (get vars string)))

(def add (evaluator +))
(def subtract (evaluator -))
(def multiply (evaluator *))
(def divide (evaluator div-n))
(defn negate [arg] #(- (arg %)))
(def arithMean (evaluator arMean))
(def geomMean (evaluator gMean))
(def harmMean (evaluator hMean))

(def map-of-functions
  {'+ add '- subtract '* multiply '/ divide 'negate negate 'arithMean arithMean 'geomMean geomMean 'harmMean harmMean})

(def parseFunction #(recursive-parser % constant variable map-of-functions))

;--------------------------------------object expression

;-----methods

(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))
(def toStringInfix (method :toStringInfix))

;-----abstracts

(defn abstract-constructor [evaluate toString toStringInfix diff]
      {:evaluate evaluate :toString toString :toStringInfix toStringInfix :diff diff})

(def abstract-operation-executor
  (abstract-constructor
    (fn [this vars] (apply (proto-get this :operation-function) (map #(evaluate % vars) (this :args))))
    (fn [this] (str "(" (proto-get this :name-string) " " (clojure.string/join " " (map #(toString %) (this :args))) ")"))
    (fn [this] (case (count (this :args))
                     1 (str (proto-get this :name-string) " " (toStringInfix (first (this :args))))
                     2 (str "(" (toStringInfix (first (this :args))) " " (proto-get this :name-string) " " (toStringInfix (second (this :args))) ")")))
    (fn [this var] ((proto-get this :operation-diff) (map #(diff % var) (this :args)) (this :args))))) ; :NOTE: дифференцировать снаружи

(defn abstract-operation [operation-function name-string operation-diff]
      (constructor
        (fn [this & args] (assoc this :args args))
        {:prototype          abstract-operation-executor
         :operation-function operation-function
         :name-string        name-string
         :operation-diff     operation-diff}))

(def abstract-unary-operation-executor
  (abstract-constructor
    (fn [this vars] ((proto-get this :operation-function) (evaluate (this :arg) vars)))
    (fn [this] (str "(" (proto-get this :name-string) " " (toString (this :arg)) ")" ))
    (fn [this] (if (proto-get this :post)
                 (str (str "(" (toStringInfix (this :arg)) " " (proto-get this :name-string) ")"))
                 (str (proto-get this :name-string) " " (toStringInfix (this :arg)))))
    (fn [this var] ((proto-get this :operation-diff) (diff (this :arg) var) (this :arg)))
    ))

(defn abstract-unary-operation [operation-function name-string post operation-diff]
      (constructor
        (fn [this arg] (assoc this :arg arg))
        {:prototype          abstract-unary-operation-executor
         :operation-function operation-function
         :name-string        name-string
         :post               post
         :operation-diff     operation-diff}))


;-----containers

(def Constant
  (constructor
    (fn [this value] (assoc this :value value))
    (abstract-constructor
      (fn [this _] (this :value))
      (fn [this] (str (this :value)))
      (fn [this] (str (this :value)))
      (fn [this _] (Constant 0)))))

(def zero (Constant 0))
(def one (Constant 1))

(def Variable
  (constructor
    (fn [this name] (assoc this :name name))
    (abstract-constructor
      (fn [this vars] (vars (clojure.string/lower-case (subs (this :name) 0 1))))
      (fn [this] (str (this :name)))
      (fn [this] (str (this :name)))
      (fn [this var] (if (= var (clojure.string/lower-case (subs (this :name) 0 1))) one zero))
      )))

;-----n-ary

(def Add (abstract-operation + '+
                             (fn [diffed-args _]
                                 (apply Add diffed-args))))

(def Subtract (abstract-operation - '-
                                  (fn [diffed-args _]
                                      (apply Subtract diffed-args))))

(def Multiply (abstract-operation * '* #(multi-diff %1 %2 Add Multiply)))

(def Divide (letfn [(temp-multi [arg] (Multiply (apply Multiply arg) (apply Multiply arg)))
                    (div-diff [diffed-args args]
                              (if (= 1 (count args))
                                (div-diff (conj diffed-args zero) (conj args one))
                                (Divide (Subtract (Multiply (first diffed-args) (apply Multiply (rest args)))
                                                  (Multiply (first args) (multi-diff (rest diffed-args) (rest args) Add Multiply)))
                                        (temp-multi (rest args)))))]
                   (abstract-operation div-n '/ div-diff)))

(def ArithMean (abstract-operation arMean 'arithMean
                                   (fn [diffed-args _]
                                       (Divide (apply Add diffed-args) (Constant (count diffed-args))))))

(def GeomMean (abstract-operation gMean 'geomMean
                                  (fn [diffed-args args] (let [len (count args)]
                                                              (Divide (Multiply (Constant (div-n len))
                                                                                (multi-diff diffed-args args Add Multiply))
                                                                      (apply Multiply
                                                                             (repeat (- len 1) (apply GeomMean args))))
                                                              ))))

(def HarmMean (abstract-operation hMean 'harmMean
                                  (fn [diffed-args args]
                                      (let [add-div-diff (apply Add (map #(Divide % (Multiply %2 %2)) diffed-args args))
                                            add-div (apply Add (map #(Divide one %) args))]
                                           (Divide (Multiply (Constant (count args)) add-div-diff)
                                                   (Multiply add-div add-div))))))

;-----binary

(def AbsC (abstract-operation (fn [y x] (Math/sqrt (+ (* x x) (* y y)))) 'absc (constantly zero)))
(def PhiC (abstract-operation (fn [y x] (Math/atan2 y x)) 'phic (constantly zero)))

(def SinC (abstract-operation (fn [y x] (* (Math/sin x) (Math/cosh y))) 'sinc (constantly zero)))
(def CosC (abstract-operation (fn [y x] (* (Math/cos x) (Math/cosh y))) 'cosc (constantly zero)))

;-----unary

(def Negate (abstract-unary-operation - 'negate false
                                      (fn [diffed-args _]
                                          (Negate diffed-args))))

(def Sin (abstract-unary-operation (fn [x] (Math/sin x)) 'sin false (constantly one)))
(def SinP (abstract-unary-operation (constantly 0) 'sinp true (constantly zero)))

(def Cos (abstract-unary-operation (fn [x] (Math/cos x)) 'cos false (constantly one)))
(def CosP (abstract-unary-operation (fn [y] (Math/cosh y)) 'cosp true (constantly zero)))


(def map-of-objects
  {'+ Add '- Subtract '* Multiply '/ Divide 'negate Negate
   'arithMean ArithMean 'geomMean GeomMean 'harmMean HarmMean
   'sin Sin 'cos Cos 'sinc SinC 'cosc CosC 'sinp SinP 'cosp CosP 'absc AbsC 'phic PhiC})

(def parseObject #(recursive-parser % Constant Variable map-of-objects))

;--------------------------------------recursive parser

(defn recursive-parser [expr-string num-container var-container map-of-operations]
      (letfn [(parse [expr]
                     (cond
                       (number? expr) (num-container expr)
                       (and (symbol? expr) (not (contains? map-of-operations expr))) (var-container (str expr))
                       (seq? expr) (apply (map-of-operations (first expr)) (map parse (rest expr)))
                       ::else ()))]
             (parse (read-string expr-string))))

;--------------------------------------combinatorial parser

(defparser parseObjectInfix
           (*operation [& symbols] (+map map-of-objects
                                         (apply +or (map #(+map symbol
                                                                (apply +seqf str (map (comp +char str) (name %))))
                                                         symbols))))

           (*pref-eval [next-priority & op-name] (+seqf #(%1 %2) (apply *operation op-name) next-priority))
           (*post-eval [next-priority & op-name] (+seqf #(%2 %1) *ws next-priority (apply *operation op-name) *ws))

           (*l-evaluator [next-priority & op-name]
                         (+map #(reduce (fn [v s] (s v)) %)
                               (+seqf cons next-priority (+star
                                                           (+seqf (fn [op r] #(op % r)) (apply *operation op-name)
                                                                  next-priority)))))
           (*r-evaluator [next-priority & op-name]
                         (+seqf #(%1 %2)
                                (+map #(apply comp %) (+star (+seqf (fn [l op] #(op l %)) next-priority (apply *operation op-name))))
                                next-priority))

           *space (+char " \t\n\r")
           *ws (+ignore (+star *space))
           *variable (+map Variable (+str (+plus (+char "xyzXYZ"))))
           *digit (+char "0123456789")
           (sign [s tail] (if (#{\- \+} s) (cons s tail) tail))
           *integer-number (+seqf sign (+opt (+char "-+")) (+plus *digit))
           *float-number (+seqf #(concat % (cons %2 %3)) *integer-number (+char ".") (+plus *digit))
           *number (+map (comp Constant read-string) (+str (+or *float-number *integer-number)))
           *module (+seqn 0 *ws (+or *variable *number (+seqn 1 (+char "(") *second-priority (+char ")"))
                                     (+seqn 1 (+char "(") *unary-c (+char ")"))  *unary) *ws)
           *complex (delay (*r-evaluator *module 'sinc 'cosc 'absc 'phic))
           *first-priority (delay (*l-evaluator *complex '* '/))
           *second-priority (delay (*l-evaluator *first-priority '+ '-))
           *unary (delay (*pref-eval *module 'negate 'cos 'sin))
           *unary-c (delay (*post-eval *module 'sinp 'cosp))
           *parseObjectInfix *second-priority)
