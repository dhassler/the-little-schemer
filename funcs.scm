(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eqan a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define rember
  (lambda (a l)
    (cond
      ((null? l) '())
      ((oequal? a (car l)) (cdr l))
      (else (cons (car l) (rember a (cdr l)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (old new lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
         (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR old new (cdr lat)))))))

(define insertL
  (lambda (old new lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
         (cons new lat))
      (else (cons (car lat) (insertL old new (cdr lat)))))))

(define subst
  (lambda (old new lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
         (cons new (cdr lat)))
      (else
        (cons (car lat) (subst old new (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1)
           (eq? (car lat) o2))
         (cons new (cdr lat)))
      (else
        (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (old new lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
         (cons (car lat) (cons new (multiinsertR old new (cdr lat)))))
      (else (cons (car lat) (multiinsertR old new (cdr lat)))))))

(define multiinsertL
  (lambda (old new lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
         (cons (new (cons old (multiinsertL (cdr old new lat))))))
      (else (cons (car lat) (multiinsertL old new (cdr lat)))))))

(define multisubst
  (lambda (old new lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
         (cons new (multisubst old new (cdr lat))))
      (else (cons (car lat) (multisubst old new (cdr lat)))))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define o+
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (o+ a (sub1 b)))))))

(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (o- a (sub1 b)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (o+ a (o* a (sub1 b)))))))

(define tup+
  (lambda (tupa tupb)
    (cond
      ((null? tupa) tupb)
      ((null? tupb) tupa)
      (else (cons
              (+ (car tupa) (car tupb))
              (tup+ (cdr tupa) (cdr tupb)))))))

(define o<
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (o< (sub1 x) (sub1 y))))))

(define o>
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (o> (sub1 x) (sub1 y))))))

(define o=
  (lambda (x y)
    (cond
      ((o< x y) #f)
      ((o> x y) #f)
      (else #t))))

(define oexp
  (lambda (x e)
    (cond
      ((eq? 0 e) 1)
      (else (o* x (oexp x (sub1 e)))))))

(define odiv
  (lambda (n d)
    (cond
      ((< n d) 0)
      (else
        (add1 (odiv (- n d) d))))))

(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (olength (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((eq? n 1) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (no-nums (cdr l)))
      (else (cons (car l) (no-nums (cdr l)))))))

(define all-nums
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (cons (car l) (all-nums (cdr l))))
      (else (all-nums (cdr l))))))

(define eqan
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one? (lambda (n) (eq? n 1)))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else
        (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (old new l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* old new (cdr l)))))
         (else (cons (car l) (insertR* old new (cdr l))))))
      (else (cons (insertR* old new (car l)) (insertR* old new (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else
        (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (old new l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* old new (cdr l))))
         (else (cons (car l) (subst* old new (cdr l))))))
      (else (cons (subst* old new (car l)) (subst* old new (cdr l)))))))

(define insertL*
  (lambda (old new l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* old new (cdr l)))))
         (else (cons (car l) (insertL* old new (cdr l))))))
      (else (cons (insertL* old new (car l)) (insertL* old new (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (oequal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(define oequal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))


(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
        (cons (car lat) (makeset2 (rember* (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or
              (member? (car set1) set2)
              (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (union (cdr set1) (cons (car set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
        (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (cond
      (else (car p)))))

(define second
  (lambda (p)
    (cond
      (else (car (cdr p))))))

(define third
  (lambda (l)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cond
      (else (cons s1 (cons s2 '()))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
        (cons (revpair (car rel))
              (revrel (cdr rel)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))
