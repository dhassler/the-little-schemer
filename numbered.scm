(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp)) (numbered (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'x) (and (numbered? (car aexp)) (numbered (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^) (and (numbered? (car aexp)) (numbered (car (cdr (cdr aexp)))))))))

(define first-sub-exp
  (lambda (l) (car (cdr l))))

(define second-sub-exp
  (lambda (l) (car (cdr (cdr l)))))

(define operator
  (lambda (l) (car l)))

(define value-old
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (+    (value (first-sub-exp nexp)) (value (second-sub-exp nexp))))
      ((eq? (operator nexp) 'x) (*    (value (first-sub-exp nexp)) (value (second-sub-exp nexp))))
      ((eq? (operator nexp) '^) (oexp (value (first-sub-exp nexp)) (value (second-sub-exp nexp)))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (first-sub-exp nexp))
             (value (second-sub-exp nexp)))))))

(define sero?
  (lambda (n) (null? n)))

(define eadd1
  (lambda (n) (cons '() n)))

(define zub1
  (lambda (n) (cdr n)))

(define eplus
  (lambda (x y)
    (cond
      ((sero? y) x)
      (else (eplus (eadd1 x) (zub1 y))))))
