
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(null? xs) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t (letrec ([len (length xs)]
                     [rem (remainder n len)]
                     [fun (lambda (xs n)
                          (cond [(= n 0) (car xs)]
                                [#t (fun (cdr xs) (- n 1))]))])
              (fun xs rem))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))
                    
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (string=? x "dog.jpg")
                    (cons "dan.jpg" (lambda () (f "dan.jpg")))
                    (cons "dog.jpg" (lambda () (f "dog.jpg")))))])
    (lambda () (f "dog.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (i)
                 (if (= i (vector-length vec))
                     #f
                     (if (and (pair? (vector-ref vec i)) (equal? (car (vector-ref vec i)) v))
                            (vector-ref vec i)
                            (f (+ i 1)))))])
           (f 0)))  

(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [ctr 0]
           [f (lambda (v)
                (let ([pair_or_false (vector-assoc v vec)])
                  (cond [pair_or_false pair_or_false]
                        [#t (let ([as (assoc v xs)])
                              (if as
                                  (begin (vector-set! vec ctr as)
                                         (set! ctr (remainder (+ ctr 1) n))
                                         as)
                                  #f))])))])
    (lambda (v) (f v))))
  

