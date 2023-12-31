(define (isqrt n0) 
  (let ((x0 (/ n0 2)) 
        (x1 (/ (+ x0 (/ n0 x0)) 2))) 
       (while (< x1 x0) 
           (set! x0 x1) 
           (set! x1 (/ (+ x0 (/ n0 x0)) 2))
        )
    x0)
)

(define (isprime n) 
    (if (< n 2) 
       #f 
       (if (== n 2) 
           #t 
           (if (== (% n 2) 0) 
               #f 
               (let ((s (isqrt n)) 
                     (d 3) 
                     (b #t)) 
                    (while (and (<= d s) b) 
                        (if (== (% n d) 0) 
                            (set! b #f) 
                            (set! d (+ d 2)))) 
                b)
           )
        ) 
    )
)

(define (primes m) 
    (let ((m0 2) 
        (pr '())) 
        (while (<= m0 m) 
            (if (isprime m0) 
                (set! pr (append pr m0)))
            (set! m0 (+ m0 1))) 
    pr )
)

(define err_count 0)
(define ok_count 0)

; There are 25 primes in [2..100]:
(if (== (length (primes 100)) 25) 
    (begin
        (print "100 first primes, OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Failure calculating 100 first primes\n") 
        (define err_count (+ err_count 1))
    )
)

; Factorial (recursion)
(define (factorial n) 
    (if (== n 0) 
        1 
        (* n (factorial (- n 1)))))

(if (== (factorial 4) 24) 
    (begin
        (print "Factorial test OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Failure calculating Factorial\n") 
        (define err_count (+ err_count 1))
    )
)

; errors

(if (== (type (/ 1 0)) 'Error)
    (begin
        (print "Type-test Error OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Failure on Type-test Error\n") 
        (define err_count (+ err_count 1))
    )
)

; cond
(if (== (let ((t 3)) (cond
   ((== t 1) "one")
   ((== t 2) "two")
   ((== t 3) "three")
   ((== t 4) "four")
   )) "three")
    (begin
        (print "Conv enum OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin
        (print "Failure on conv enum\n") 
        (define err_count (+ err_count 1))
    )
)

; types
(if (== (convtype "1" 'Int) 1)
    (begin
        (print "Convtype OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin
        (print "Failure on convtype str->int Error\n") 
        (define err_count (+ err_count 1))
    )
)

; (let ((cv '('Int 'Float))) (map (lambda (x) (convtype 1 x)) cv)) 

(if (== ((convtype "+" 'Symbol) 2 3) 5)
    (begin
        (print "Convtype indirect function generation OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Failure on Convtype indirect function generation\n") 
        (define err_count (+ err_count 1))
    )
)

(if (== (((convtype "convtype" 'Symbol) "+" 'Symbol) 2 3) 5)
    (begin
       (print "Convtype 2 x indirect function generation OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Failure on Convtype 2 x indirect function generation\n") 
        (define err_count (+ err_count 1))
    )
)

(let ((source '(1 2 3 4 1.0 2.0 3.0 4.0))
      (dest '('Int 'Float 'String 'Boolean 'Int 'Float 'String 'Boolean))
      (res '(1 2.00000 "3" #t 1 2.00000 "3.000000" #t)))
      (map (lambda (x y z)
          (begin
              ; XXX open issue: printing the type directly loses a '() mem
              ; mem-loss on y: (print " " x y z)
              ; ok: (print x ": " (stringify y) "->" z "\n")
              (if (== (convtype x y) z)
                  (begin
                      (print "Convtype map array OK\n")
                      (define ok_count (+ ok_count 1))
                  ) 
                  (begin 
                      (print "Failure on Convtype map array\n") 
                      (define err_count (+ err_count 1))
                  )
               )
           ))
           source dest res
       )
)
      

; Lists
(let ((r (range 4)) (sum 0))
    (every 
        (lambda (x) (set! sum (+ sum x)))
        r
    )
    (if (== sum 6)
        (begin
            (print "Range-sum test OK\n")
            (define ok_count (+ ok_count 1))
        ) 
        (begin 
            (print "Failure calculating Range-sum " sum 6 "\n") 
            (define err_count (+ err_count 1))
        )
    )
)

(if (== (car (cdr (cdr (cdr (list 'a 'b 'c 'd 'e))))) 'd)
    (begin
        (print "car-cdr test OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Failure on car-cdr\n")
        (print (car (cdr (cdr (cdr (list 'a 'b 'c 'd 'e))))) 'd)
        (define err_count (+ err_count 1))
    )
)

(let ((t '(a b c)))
    (if (and 
            (== (stringify (cons (car t) (cdr t))) "(a b c)")
            #t
            ;(== (length (stringify (cons (car t) (cdr t)))) (length t))
        )   
        (begin
            (print "stringify-cons-car-cdr test OK\n")
            (define ok_count (+ ok_count 1))
        ) 
        (begin 
            (print "Failure on stringify-cons-car-cdr\n")
            (print (stringify (cons (car t) (cdr t))))
            (define err_count (+ err_count 1))
        )
    )
)

(let ((t '(a b c d)) (tok 'c))
    (let ((ind (find t tok)) (res (index t ind)))
        (if (== tok res)
            (begin
                (print "find-index test OK\n")
                (define ok_count (+ ok_count 1))
            ) 
            (begin 
                (print "Failure on find-index\n")
                (print res "<->" tok)
                (define err_count (+ err_count 1))
            )
        )
    )
)

; String stuff
(let ((s "Hello, world!") (tok "world"))
    (if (== (substring s (find s tok)) "world!")
        (begin
            (print "substring test 1 OK\n")
            (define ok_count (+ ok_count 1))
        ) 
        (begin 
            (print "Failure on substring test 1\n")
            (define err_count (+ err_count 1))
        )
    )
    (if (== (substring s 1 2) "el")
        (begin
            (print "substring test 2 OK\n")
            (define ok_count (+ ok_count 1))
        ) 
        (begin 
            (print "Failure on substring test 2\n")
            (define err_count (+ err_count 1))
        )
    )
)

(if (== (+ "Hello," " world!") "Hello, world!")
    (begin
        (print "string test add OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin
        (print "Failure on string test add\n")
        (define err_count (+ err_count 1))
    )
)

(define (join stringlist)
    (let ((js ""))
        (every (lambda (x) (set! js (+ js x))) stringlist)
        js
    )
)

(let ((test "Hello, world!"))
    (if (== test (join (splitstring test)))
        (begin
            (print "string split-join OK\n")
            (define ok_count (+ ok_count 1))
        ) 
        (begin
            (print "Failure on string split-join: " test "->" (join (splitstring test))  "\n")
            (define err_count (+ err_count 1))
        )
    )
)

(if (== (join '("a" "b")) "ab")
    (begin
        (print "string join manual list OK\n")
        (define ok_count (+ ok_count 1))
    ) 
    (begin
        (print "Failure on string join manual list\n")
        (define err_count (+ err_count 1))
    )
)

; Lambda stuff
(if (== ((lambda (x) (+ x 7)) 8) 15) 
    (begin
        (print "Lambda var1 OK\n") 
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Lambda var1 ERROR\n")
        (define err_count (+ err_count 1))
    )
)

(if (== ((lambda (x y) (+ x y 7)) 8 9) 24) 
    (begin
        (print "Lambda var2 OK\n") 
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Lambda var2 ERROR\n")
        (define err_count (+ err_count 1))
    )
)

(if (== ((lambda (x y) (x y 7 8)) '+ 9) 24) 
    (begin
        (print "Lambda op-var var1 OK\n") 
        (define ok_count (+ ok_count 1))
    ) 
    (begin 
        (print "Lambda op-var var1 ERROR\n")
        (define err_count (+ err_count 1))
    )
)

(let ((n 1) (m 2) (t 0)) 
    (while (< n 5) 
        (set! n (+ n 1)) 
        (if (== n m) 
            (set! t (+ t 1))
        )
    )
    (if (== t 1) 
        (begin
            (print "While OK\n")
            (define ok_count (+ ok_count 1))
        ) 
        (begin 
            (print "While ERROR\n")
            (define err_count (+ err_count 1))
        )
    )
)

(let ((l1 '(1 2 3)) (l2 '(3 4 5)))
    (let ((sum 0))
        (every (lambda (x) (set! sum (+ sum x)))
            (map * l1 l2)
        )
        (if (== sum 26) 
            (begin
                (print "Lambda mapper OK\n")
                (define ok_count (+ ok_count 1))
            ) 
            (begin 
                (print "Lambda mapper failed: sum=" sum "\n")
                (define err_count (+ err_count 1))
            )
        )
    )
)

(print "--------------------------------------------\n")
(print " Test OK:  " ok_count "\n")
(print " Test ERR: " err_count "\n")
(if (> err_count 0) 
    (print " TEST FAILURE(s)\n") 
    (print " ALL OK!\n"))
(print "--------------------------------------------\n")
