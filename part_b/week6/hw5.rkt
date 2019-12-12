;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
;;(a)
(define (racketlist->mupllist rlst)
  (if (null? rlst)
      (aunit)
      (apair (car rlst) (racketlist->mupllist (cdr rlst)))))

;;(b)
(define (mupllist->racketlist mlst)
  (if (aunit? mlst)
      null
      (cons (apair-e1 mlst) (mupllist->racketlist (apair-e2 mlst)))))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env)  (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (begin (println "#t") (envlookup (cdr env) str))]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e)
         e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]
               [v3 (eval-under-env (ifgreater-e3 e) env)]
               [v4 (eval-under-env (ifgreater-e4 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   v3
                   v4)
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([ex (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) ex) env)))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([ex (eval-under-env (fst-e e) env)])
           (if (apair? ex)
               (apair-e1 ex)
               (error "fst applied to non-apair")))]
        [(snd? e)
         (let ([ex (eval-under-env (snd-e e) env)])
           (if (apair? ex)
               (apair-e2 ex)
               (error "snd applied to non-apair")))]
        [(aunit? e)
         e]
        [(isaunit? e)
         (if (aunit? e)
             (int 1)
             (int 0))]
        [(fun? e)
         (closure env e)]
        [(closure? e)
         e]
        [(call? e)
         (let ([cls (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cls)
               (let* ([fun_name (fun-nameopt (closure-fun cls))]
                     [fun_arg (fun-formal (closure-fun cls))]
                     [fun_body (fun-body (closure-fun cls))]
                     [arg_to_env (cons fun_arg arg)]
                     [fun_to_env (cons fun_name cls)])
                 (begin (println (cons fun_to_env (cons arg_to_env (cons arg_to_env env)))))
                 (eval-under-env fun_body (cons fun_to_env (cons arg_to_env (cons arg_to_env env)))))
               (error "call's first argument not a closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (isaunit? e1)
      e2
      e3))

(define (mlet* lstlst e2)
  (eval-under-env e2 lstlst))

(define (ifeq e1 e2 e3 e4)
  (if (and (int? e1) (int? e2))
      (if (= (int-num e1) (int-num e2))
          e3
          e4)
      (error "ifeq e1 or e2 in not of type int")))

;; Problem 4
;;(struct fun  (nameopt formal body) #:transparent)
;(struct closure (env fun) #:transparent)
;(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
;> (eval-exp (call (eval-exp (call (eval-exp (fun "fmf" "mf" (fun "fml" "ml" (int 1)))) (int 2))) (int 3)))
(define mupl-map
  (fun "map_f" "f" (if (int? (var "f")) (var "f") (var "f"))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
