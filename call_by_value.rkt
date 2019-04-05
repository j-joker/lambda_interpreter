#lang racket
(define (atom? exp) (not (pair? exp)))
(define env '())
(struct Closure (f env))
(define ext-env 
  (lambda (x v env)
    (cons `(,x . ,v) env)
    )
  )
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond 
        [(not p) x]
        [else (cdr p)]
        )
      )
    )
  )
(define (evaluate exp env)
  (match exp
    [(? symbol?) (lookup exp env)]
    [(? atom?) exp]
    [`(quote ,x) (cadr exp)]
    [`(if ,test ,th ,el)
     (if (evaluate test env)
         (evaluate  th env)
         (evaluate  el env)
         )
     ]
   [`(lambda (,x) ,exp) 
    (Closure `(lambda (,x) ,exp) env)
    ]
   [`(,op ,e1 ,e2)
    (match op
      ['+ (+ (evaluate e1 env) (evaluate e2 env))]
      ['- (- (evaluate e1 env) (evaluate e2 env))]
      )
    ]
   [`(,rator ,rand)
    (match rator
      [`(lambda (,x) ,exp)
       (let ([val (evaluate rand env)])
         (evaluate exp (ext-env x val env))
         )
       ]
      )
    ]
    )
  )
(set! env (ext-env 'a 1 env))
(evaluate ((lambda (x) (+ x 5)) 1) env)


