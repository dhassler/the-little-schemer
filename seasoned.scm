
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? a (car lat))))))


(define sum-of-prefixes-b
  (lambda (total tup)
    (cond
      ((null? tup) '())
      (else (cons (+ total (car tup))
                  (sum-of-prefixes-b
                    (+ total (car tup))
                    (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
      (sum-of-prefixes-b 0 tup)))

(define sub1 (lambda (n) (- n 1)))

(define pick
  (lambda (n lat)
    (cond
      ((eq? 1 n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define scramble-b
  (lambda (tup revpre)
    (cond
      ((null? tup) '())
      (else
        (cons (pick (car tup)
                    (cons (car tup) revpre))
              (scramble-b (cdr tup) (cons (car tup) revpre)))))))

(define scramble
  (lambda (tup) (scramble-b tup '())))

(define multirember
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

(define rember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) (cdr lat))
        (else (cons (car lat) ((rember-f test?) a (cdr lat))))))))

(define rember (rember-f eq?))

(define multirember-f
  (lambda (test?)
    (letrec
       ((m-f (lambda (a lat)
              (cond
                ((null? lat) '())
                ((test? a (car lat)) (m-f a (cdr lat)))
                (else (cons (car lat) (m-f a (cdr lat))))))))
       m-f)))

(define member?
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
              (cond
                ((null? l) #f)
                ((eq? (car l) a) #t)
                (else (yes? (cdr l)))))))
      (yes? lat))))

(define union
  (lambda (lat1 lat2)
    (letrec ((U (lambda (l)
                  (cond
                    ((null? l) lat2)
                    ((member? (car l) lat2) (union (cdr l) lat2))
                    (else (union (cdr l) (cons (car l) lat2)))))))
      (U lat1))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define two-in-a-row-b?
  (letrec ((W
    (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      ((eq? preceding (car lat)) #t)
      (else (W (car lat) (cdr lat)))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

(define intersect
  (lambda (set1 set2)
    (letrec ((I
               (lambda (s1)
                 (cond
                   ((null? s1) '())
                   ((member? (car s1) set2)
                    (cons (car s1) (I (cdr s1))))
                   (else (I (cdr s1)))))))
      (I set1))))

(define-syntax letcc
  (syntax-rules ()
                ((letcc var body ...)
                 (call-with-current-continuation
                   (lambda (var)  body ... )))))

(define-syntax try
  (syntax-rules ()
                ((try var a . b)
                 (letcc success
                        (letcc var (success a)) . b))))

(define intersectall
  (lambda (lset)
    (letcc hop
           (letrec
             ((A (lambda (lset)
                   (cond
                     ((null? (car lset)) (hop '()))
                     ((null? (cdr lset)) (car lset))
                     (else (I (car lset) (A (cdr lset)))))))
              (I (lambda (set1 set2)
                   (letrec
                     ((J (lambda (set1)
                           (cond
                             ((null? set1) '())
                             ((member? (car set1) set2)
                              (cons (car set1) (J (cdr set1))))
                             (else (J (cdr set1)))))))
                     (cond
                       ((null? set2) (hop '()))
                       (else (J set1)))))))
             (cond
               ((null? lset) '())
               (else (A lset)))))))

(define rember
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
          (cond
            ((null? lat) '())
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (letcc skip
           (letrec
             ((R (lambda (lat)
                   (cond
                     ((null? lat) '())
                     ((eq? (car lat) a)
                      (skip (R (cdr lat))))
                     (else (cons (car lat) (R (cdr lat))))))))
             (R lat)))))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
        (let ((a (leftmost (car l))))
          (cond
            ((atom? a) a)
            (else (leftmost (cdr l)))))))))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (max (add1 (depth* (car l)))
             (depth* (cdr l)))))))

(define leftmost
  (lambda (l)
    (letcc skip
           (letrec ((lm
                      (lambda (l)
                        (cond
                          ((null? l) '())
                          ((atom? (car l)) (skip (car l)))
                          (else (let ()
                           (lm (car l))
                           (lm (cdr l))))))))
            (lm l)))))

(define rember1*
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else (cons (car l) (R (cdr l))))))
              (else
                (let ((RL (R (car l))))
                  (cond
                    ((eqlist? RL (car l))
                     (cons (car l) (R (cdr l))))
                    (else (cons RL (cdr l))))))))))
      (R l))))

(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh 'no))
      ((atom? (car l))
       (if (eq? (car l) a)
         (cdr l)
         (cons (car l) (rm a (cdr l) oh))))
      (else
        (letcc)
