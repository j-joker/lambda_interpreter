#lang racket


(define env0 
  '((zero? . (int -> bool))
    (add1 . (int -> int))
    (sub1 . (int -> int))
    (= . (int -> (int -> bool)))
    (<= . (int -> (int -> bool)))
    (+ . (int -> (int -> int)))
    (- . (int -> (int -> int)))
    (* . (int -> (int -> int)))
    (/ . (int -> (int -> int)))
    ))
(define  s0 '())
(define lookup
  (lambda (x env)
    (let ([slot (assq x env)])
      (cond
        [(not slot) (error 'lookup "unbound variable ~a" x)]
        [else (cdr slot)]
        )
      )
    )
  )
(define  (var x) (vector x))
(define  (var? x) (vector? x))
(define ext-env
  (lambda (x t env)
    (cons `(,x . ,t) env)
    )
  )
(define walk 
  (lambda (u s)
    (let ([r (assq u s)])
      (cond 
        [(not r) u]
        [(var? (cdr r)) (walk (cdr r) s)]
        [else (cdr r)]
        )
      )
    )
  )

(define unify
  
  (lambda (u v s)
    (let ([u* (walk u s)]
          [v* (walk v s)]
          )
      (cond 
        [(eq? u* v*) s]
        [(var? u*)
         (cons `(,u* . ,v*) s)
         ]
        [(var? v*)
         (cons `(,v* . ,u*) s)
         ]
        [(and (pair? u*) (pair? v*))
         (let ([r (unify (car u*) (car v*) s)])
           (cond
             [r (unify (cdr u*) (cdr v*) r)]
             [else false]
             )
           )
         ]
        [else false]
        )
      )
    )
  )

(define reify-name
  (lambda (n)
    (string->symbol 
      (string-append "t" (number->string n)))
    )
  )
(define reify 
  (lambda (x s)
    (define reify1
      (lambda (x n s)
        (let ([x (walk x s)])
          (cond
            [(pair? x)
             (define-values (u n1 s1) (reify1 (car x) n s))
             (define-values (v n2 s2) (reify1 (cdr x) n1 s1))
             (values (cons u v) n2 s2)
             ]
            [(var? x) 
             (define v* (reify-name n))
             (values v* (add1 n) (ext-env x v* s))
             ]
             [else (values x n s)]
            )
          )
        )
      )
    (define-values (x* n* s*) (reify1 x 0 s))
    x*
    )
  )


(define	 infer 
         (lambda (exp)
           (define infer1 
             (lambda (exp env s)
               (match exp
                 [(? number?) (values 'int s)]
                 [(? boolean?) (values 'bool s)]
                 [(? string?) (values 'string s)]
                 [(? symbol?) (values (lookup exp env) s)]
                 [`(lambda (,x) ,body)
                  (define t1 (var x))
                  (define env* (ext-env x t1 env))
                  (define-values (t2 s^) (infer1 body env* s))
                  (values `(,t1 -> ,t2) s^)
                  ]
                 [`(,e1 ,e2)
                  (define-values (t1 s1) (infer1 e1 env s))
                  (define-values (t2 s2) (infer1 e2 env s1))
                  (define t3 (var 't3))
                  (define t4 (var 't4))
                  (define s3 (unify t1 `(,t3 -> ,t4) s2))
                  (cond
                    [(not s3) (error 'infer "~a is not a function" e1)]
                    [else
                     (define s4 (unify t3 t2 s3))
                     (if s4
                         (values t4 s4)
                         (error 'infer  "wrong argumen type")
                         )
                     ]
                    )
                  ]
                 )
               )
             )
           (define-values (t s) (infer1 exp env0 s0))
           (reify t s)
           )
         )

; (infer 1)
(infer '(lambda (v) v))
(infer '(lambda (f) (lambda (x) (f x))))
(infer '((lambda (f) (lambda (x) (f (f x)))) add1))
