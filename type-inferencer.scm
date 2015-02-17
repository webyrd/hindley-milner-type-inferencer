(load "mk.scm")
(load "test-check.scm")

(define (!-o gamma expr type)
  (conde
    ((symbolo expr)
     (lookupo gamma expr type))
    ((numbero expr)
     (== 'int type))       
    ((== #f expr)
     (== 'bool type))       
    ((== #t expr)
     (== 'bool type))
    ((fresh (x e T1 T2)
       (== `(lambda (,x) ,e) expr)
       (== `(,T1 -> ,T2) type)
       (symbolo x)
       (!-o `((,x : ,T1) . ,gamma) e T2)))
    ((fresh (f e e^ t-ignore)
       (== `(let ((,f ,e)) ,e^) expr)
       (symbolo f)
       (!-o `((,f poly ,e ,gamma) . ,gamma) e^ type)
       (!-o gamma e t-ignore)))
    ((fresh (e1 e2 T)
       (== `(,e1 ,e2) expr)
       (!-o gamma e1 `(,T -> ,type))
       (!-o gamma e2 T)))
    ((fresh (e1 e2)
       (== `(+ ,e1 ,e2) expr)
       (== 'int type)
       (!-o gamma e1 'int)
       (!-o gamma e2 'int)))
    ((fresh (e1 e2 T1 T2)
       (== `(cons ,e1 ,e2) expr)
       (== `(pair ,T1 ,T2) type)
       (!-o gamma e1 T1)
       (!-o gamma e2 T2)))       
    ((fresh (e1 e2 e3)
       (== `(if ,e1 ,e2 ,e3) expr)
       (!-o gamma e1 'bool)
       (!-o gamma e2 type)
       (!-o gamma e3 type)))))

(define (lookupo gamma x t)
  (fresh ()
    (symbolo x)
    (conde
      ((fresh (e gamma^ _)
         (== `((,x poly ,e ,gamma^) . ,_) gamma)
         (!-o gamma^ e t)))
      ((fresh (_)
         (== `((,x : ,t) . ,_) gamma)))                         
      ((fresh (y _ gamma^)
         (== `((,y . ,_) . ,gamma^) gamma)
         (=/= x y)
         (symbolo y)
         (lookupo gamma^ x t))))))










(test "!-1"
  (run* (q) (!-o '() '(lambda (y) y) q))
  '((_.0 -> _.0)))

(test "!-2"
  (run* (q) (!-o '() '((lambda (y) y) (lambda (z) z)) q))
  '((_.0 -> _.0)))

(test "!-3"
  (run* (q) (!-o '() '((lambda (y) y) (lambda (z) (lambda (w) (w z)))) q))
  '((_.0 -> ((_.0 -> _.1) -> _.1))))

(test "!-4"
  (run* (q) (!-o '() '(lambda (y) (y y)) q))
  '())

(test "!-5"
  (run* (q) (!-o '() '5 q))
  '(int))

(test "!-6"
  (run* (q) (!-o '() '#t q))
  '(bool))

(test "!-10"
  (run* (q) (!-o '() '(if #t 3 4) q))
  '(int))

(test "!-pair-1"
  (run* (q) (!-o '() '(cons 3 #t) q))
  '((pair int bool)))

(test "!-pair-4"
  (run* (q) (!-o '() '(cons (cons #f 6) (cons 3 #t)) q))
  '((pair (pair bool int) (pair int bool))))

(test "!-pair-100-let"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f (cons (f 5) (f #t)))) q))
  '((pair int bool)))

(test "!-pair-100-lambda"
  (run* (q) (!-o '() '((lambda (f) (f (cons (f 5) (f #t)))) (lambda (x) x)) q))
  '())

(test "!-12"
  (run* (q) (!-o '() '(let ((x 3)) #t) q))
  '(bool))
  
(test "!-13"
  (run* (q) (!-o '() '(let ((x 3)) x) q))
  '(int))
  
(test "!-14"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f 5)) q))
  '(int))

(test "!-16"
  (run* (q) (!-o '() '(let ((f (lambda (x) (x x)))) 3) q))
  '())
  
(test "!-18"
;;; test from http://okmij.org/ftp/ML/generalization.html
  (run* (q) (!-o '() '(lambda (x) (let ((y (lambda (z) z))) y)) q))
  '((_.0 -> (_.1 -> _.1))))

(test "!-15a"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f 5)) q))
  '(int))

(test "!-15b"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f #t)) q))
  '(bool))

(test "!-15c-pair"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f (cons 5 #t))) q))
  '((pair int bool)))

(test "!-15d-pair"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f (f (cons 5 #t)))) q))
  '((pair int bool)))

  (test "!-15h-let"
    (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f (f 5))) q))
    '(int))

  (test "!-15h-lambda"
    (run* (q) (!-o '() '((lambda (f) (f (f 5))) (lambda (x) x)) q))
    '(int))

  (test "!-15f"
    (run* (q) (!-o '() '(let ((f (lambda (x) x))) (if (f 5) (f 6) (f 7))) q))
    '())

  (test "!-15f2-let"
    (run* (q) (!-o '() '(let ((f (lambda (x) x))) (if (f #t) (f 6) (f 7))) q))
    '(int))

  (test "!-15f2-lambda"
    (run* (q) (!-o '() '((lambda (f) (if (f #t) (f 6) (f 7))) (lambda (x) x)) q))
    '())

  (test "!-15f3-let"
    (run* (q) (!-o '() '(let ((f (lambda (x) x))) (if #t (f 6) (f 7))) q))
    '(int))

  (test "!-15f3-lambda"
    (run* (q) (!-o '() '((lambda (f) (if #t (f 6) (f 7))) (lambda (x) x)) q))
    '(int))

(test "!-15g"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (if (f #t) (f 6) (f 7))) q))
  '(int))

(test "!-15-pair-let"
  (run* (q) (!-o '() '(let ((f (lambda (x) x))) (f (cons (f 5) (f #t)))) q))
  '((pair int bool)))

(test "!-15-pair-lambda"
  (run* (q) (!-o '() '((lambda (f) (f (cons (f 5) (f #t)))) (lambda (x) x)) q))
  '())

(test "!-let-env-1"
  (run* (q) (!-o '() '(let ((x #t)) (let ((f (lambda (a) a))) (let ((y 7)) (let ((x 5)) (+ (f x) (f y)))))) q))
  '(int))

(test "!-let-env-2"
  (run* (q) (!-o '() '(let ((x #t)) (let ((f (lambda (a) a))) (let ((y 7)) (+ (f x) (f y))))) q))
  '())

(test "!-let-env-3"
  (run* (q) (!-o '() '(let ((x 5)) (let ((f (lambda (a) a))) (let ((y 7)) (+ (f x) (f y))))) q))
  '(int))

(test "!-let-env-4"
  (run* (q) (!-o '() '(let ((x 5)) (let ((f (lambda (a) x))) (let ((y #t)) (+ (f x) (f y))))) q))
  '(int))

(test "!-let-env-5"
  (run* (q) (!-o '() '(let ((f 5)) (let ((f f)) f)) q))
  '(int))

(test "!-let-env-6"
  (run* (q) (!-o '() '(let ((x 5)) (let ((f (lambda (x) x))) (f #f))) q))
  '(bool))

(test "!-let-env-7a"
  (run* (q) (!-o '() '(let ((x 5)) (let ((f (lambda (y) x))) (let ((x #t)) (f x)))) q))
  '(int))

(test "!-let-env-7b"
  (run* (q) (!-o '() '((lambda (x) (let ((f (lambda (y) x))) (let ((x #t)) (f x)))) 5) q))
  '(int))

(test "!-let-env-7c"
  (run* (q) (!-o '() '((lambda (x) (let ((f (lambda (y) x))) ((lambda (x) (f x)) #t))) 5) q))
  '(int))

(test "!-let-env-7d"
  (run* (q) (!-o '() '((lambda (x) ((lambda (f) ((lambda (x) (f x)) #t)) (lambda (y) x))) 5) q))
  '(int))


;;; Tests from https://github.com/namin/TAPL-in-miniKanren-cKanren-core.logic/blob/master/clojure-tapl/tapl/test/tapl/test/letpoly.clj  
  (test "!-40"
    (run* (q) (!-o '() '(lambda (x) (lambda (y) (x y))) q))
    '(((_.0 -> _.1) -> (_.0 -> _.1))))

  (test "!-41"
    (run* (q) (!-o '() '(lambda (f) (lambda (a) ((lambda (d) f) (f a)))) q))
    '(((_.0 -> _.1) -> (_.0 -> (_.0 -> _.1)))))

  (test "!-42"
    (run* (q) (!-o '() '(let ((a (lambda (a) a))) a) q))
    '((_.0 -> _.0)))

  (test "!-43"
    (run* (q) (!-o '() '(let ((a (lambda (a) a))) (a a)) q))
    '((_.0 -> _.0)))

  (test "!-44"
    (run* (q) (!-o '() '(lambda (a) (let ((b a)) b)) q))
    '((_.0 -> _.0)))

  (test "!-45"
    (run* (q) (!-o '() '(lambda (f) (lambda (a) (let ((id (lambda (x) x))) ((lambda (d) (id f)) ((id f) (id a)))))) q))
    '(((_.0 -> _.1) -> (_.0 -> (_.0 -> _.1)))))

  (test "!-46"
    (run* (q) (!-o '() '(lambda (f) (lambda (a) (let ((id (lambda (a) a))) ((lambda (d) (id f)) ((id f) (id a)))))) q))
    '(((_.0 -> _.1) -> (_.0 -> (_.0 -> _.1)))))
  
  (test "!-21"
    (run* (q) (!-o '() '(let ((f (lambda (x) x))) f) q))
    '((_.0 -> _.0)))
  
  (test "!-19"
    (run 1 (q) (fresh (lam) (== `(let ((f ,lam)) (f (f 5))) q)) (!-o '() q 'int))
    '(((let ((f (lambda (_.0) _.1))) (f (f 5))) (num _.1) (sym _.0))))

  (test "!-20"
    (run 1 (q) (fresh (lam) (== `(let ((f ,lam)) (if (f #t) (f 6) (f 7))) q)) (!-o '() q 'int))
    '(((let ((f (lambda (_.0) _.0))) (if (f #t) (f 6) (f 7))) (sym _.0))))
  
  (test "!-23"
;;; self-application via let polymorphism.  I guess that's a thing???
    (run* (q)
      (!-o '() '(let ((f (lambda (x) 5))) (f f)) q))
    '(int))

  (test "!-23b"
;;; self-application without let poly doesn't type check!
    (run* (q)
      (!-o '() '((lambda (f) (f f)) (lambda (x) 5)) q))
    '())

  (test "!-23c"
    (run* (q)
      (!-o '() '((lambda (x) (x x)) (lambda (x) (x x))) q))
    '())

  (test "!-23d"
;;; self-application via let polymorphism.  I guess that's a thing???    
    (run* (q)
      (!-o '() '(let ((f (lambda (x) x))) (f f)) q))
    '((_.0 -> _.0)))

  (test "!-23e"
;;; self-application without let poly doesn't type check!    
    (run* (q)
      (!-o '() '((lambda (f) (f f)) (lambda (x) x)) q))
    '())

  (test "!-23f"
;;; omega still doesn't typecheck    
    (run* (q)
      (!-o '() '(let ((f (lambda (x) (x x)))) (f f)) q))
    '())
  
  (test "!-23g"
    (run* (q)
      (!-o '() '((lambda (x) (x x)) (lambda (x) (x x))) q))
    '())

  (test "!-23h"
    (run* (q)
      (!-o '() '(let ((f (lambda (x) (x 5)))) (f f)) q))
    '())
  
  
  (test "!-29"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             5)
          q))
    '(int))

  (test "!-30"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) y)))
               5))
          q))
    '(int))

  (test "!-31"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) y)))
               (f1 5)))
          q))
    '(int))

  (test "!-37"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) y)))
               f1))
          q))
    '((_.0 -> _.0)))
  
  (test "!-36"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) y)))
               f0))
          q))
    '((_.0 -> _.0)))
  
  (test "!-32"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) y)))
               (f0 5)))
          q))
    '(int))

  (test "!-33"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) y)))
               (f0 (f1 5))))
          q))
    '(int))

  (test "!-34"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) y)))
               (f0 (f0 (f1 (f1 (f0 (f1 (f0 5)))))))))
          q))
    '(int))
  
  (test "!-28"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) f0)))
               5))
          q))
    '(int))
  
  (test "!-27"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 y))))
               5))
          q))
    '(int))
  
  (test "!-26"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               5))
          q))
    '(int))
  
  (test "!-25"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               f1))
          q))
    '((_.0 -> _.0)))

  (test "!-25a"
    (run 1 (q)
      (fresh (lam)
        (== `(let ((f ,lam)) (f (cons (f 5) (f #t)))) q)
        (!-o '() q '(pair int bool))))
    '(((let ((f (lambda (_.0) _.0))) (f (cons (f 5) (f #t)))) (sym _.0))))

  (test "!-25b"
    (run 10 (q)
      (fresh (lam)
        (== `(let ((f ,lam)) (f (cons (f 5) (f #t)))) q)
        (!-o '() q '(pair int bool))))
    '(((let ((f (lambda (_.0) _.0))) (f (cons (f 5) (f #t))))
       (sym _.0))
      ((let ((f (lambda (_.0) (cons _.1 #f))))
         (f (cons (f 5) (f #t))))
       (num _.1) (sym _.0))
      ((let ((f (lambda (_.0) (let ((_.1 _.2)) _.0))))
         (f (cons (f 5) (f #t))))
       (=/= ((_.0 _.1))) (num _.2) (sym _.0 _.1))
      ((let ((f (lambda (_.0) (let ((_.1 #f)) _.0))))
         (f (cons (f 5) (f #t))))
       (=/= ((_.0 _.1))) (sym _.0 _.1))
      ((let ((f (lambda (_.0) (cons _.1 #t))))
         (f (cons (f 5) (f #t))))
       (num _.1) (sym _.0))
      ((let ((f (lambda (_.0) (let ((_.1 #t)) _.0))))
         (f (cons (f 5) (f #t))))
       (=/= ((_.0 _.1))) (sym _.0 _.1))
      ((let ((f (let ((_.0 _.1)) (lambda (_.2) _.2))))
         (f (cons (f 5) (f #t))))
       (num _.1) (sym _.0 _.2))
      ((let ((f (let ((_.0 #f)) (lambda (_.1) _.1))))
         (f (cons (f 5) (f #t))))
       (sym _.0 _.1))
      ((let ((f (let ((_.0 #t)) (lambda (_.1) _.1))))
         (f (cons (f 5) (f #t))))
       (sym _.0 _.1))
      ((let ((f (lambda (_.0) (let ((_.1 _.0)) _.0))))
         (f (cons (f 5) (f #t))))
       (=/= ((_.0 _.1))) (sym _.0 _.1))))

  (test "!-30a"
    (run* (q)
      (!-o '() '(let ((f0 (lambda (x) (cons x x))))
                 (f0 (lambda (z) z))) q))
    '((pair (_.0 -> _.0) (_.0 -> _.0))))

  (test "!-30b"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) (cons x x))))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (f1 (lambda (z) z))))
          q))
    '((pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))

  (test "!-30c"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) (cons x x))))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (let ((f2 (lambda (y) (f1 (f1 y)))))
                 (f2 (lambda (z) z)))))
          q))
    '((pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))))

  (test "!-30d"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) (cons x x))))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (let ((f2 (lambda (y) (f1 (f1 y)))))
                 (let ((f3 (lambda (y) (f2 (f2 y)))))
                   (f3 (lambda (z) z))))))
          q))
    '((pair (pair (pair (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))) (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))))) (pair (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))) (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))))) (pair (pair (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))) (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))))) (pair (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))) (pair (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))))) (pair (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))) (pair (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0))) (pair (pair (_.0 -> _.0) (_.0 -> _.0)) (pair (_.0 -> _.0) (_.0 -> _.0)))))))))))

;;; This test returns after several minutes, producing an absurdly gigantic type!
#|
(test "!-30e"
  (run* (q)
    (!-o '()
         '(let ((f0 (lambda (x) (cons x x))))
            (let ((f1 (lambda (y) (f0 (f0 y)))))
              (let ((f2 (lambda (y) (f1 (f1 y)))))
                (let ((f3 (lambda (y) (f2 (f2 y)))))
                  (let ((f4 (lambda (y) (f3 (f3 y)))))
                    (f4 (lambda (z) z)))))))
         q))
  '???)
|#

  (test "!-24a"
    (run* (q)
      (!-o '() '(let ((f0 (lambda (x) x)))
                 (f0 (lambda (z) z))) q))
    '((_.0 -> _.0)))

  (test "!-24b"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (f1 (lambda (z) z))))
          q))
    '((_.0 -> _.0)))

  (test "!-24c"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (let ((f2 (lambda (y) (f1 (f1 y)))))
                 (f2 (lambda (z) z)))))
          q))
    '((_.0 -> _.0)))

  (test "!-24d"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (let ((f2 (lambda (y) (f1 (f1 y)))))
                 (let ((f3 (lambda (y) (f2 (f2 y)))))
                   (f3 (lambda (z) z))))))
          q))
    '((_.0 -> _.0)))

#!eof

;; these tests take too long to terminate!

  (test "!-24e"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (let ((f2 (lambda (y) (f1 (f1 y)))))
                 (let ((f3 (lambda (y) (f2 (f2 y)))))
                   (let ((f4 (lambda (y) (f3 (f3 y)))))
                     (f4 (lambda (z) z)))))))
          q))
    '((_.0 -> _.0)))  

  (test "!-24f"
    (run* (q)
      (!-o '()
          '(let ((f0 (lambda (x) x)))
             (let ((f1 (lambda (y) (f0 (f0 y)))))
               (let ((f2 (lambda (y) (f1 (f1 y)))))
                 (let ((f3 (lambda (y) (f2 (f2 y)))))
                   (let ((f4 (lambda (y) (f3 (f3 y)))))
                     (let ((f5 (lambda (y) (f4 (f4 y)))))
                       (f5 (lambda (z) z))))))))
          q))
    '((_.0 -> _.0)))
