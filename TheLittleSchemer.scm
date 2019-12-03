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
         (else (or (eq? (car lat) a)
                   (member? a (cdr lat)))))))

(define (rember a lat)
   (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else
       (cons (car lat) (rember a (cdr lat))))))

(define (firsts lol)
   (cond ((null? lol) '())
         ((list? (car lol)) 
          (cons (car (car lol)) 
                (firsts (cdr lol))))))

(define insertR
   (lambda (new old lat)
      (cond
         ((null? lat) '())
         ((eq? (car lat) old)
          (cons old (cons new (cdr lat))))
         (else
          (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
   (lambda (new old lat)
      (cond
         ((null? lat) '())
         ((eq? (car lat) old)
          (cons new lat))
         (else
          (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
   (lambda (new old lat)
      (cond
         ((null? lat) '())
         ((eq? (car lat) old)
          (cons new (cdr lat)))
         (else
          (cons (car lat) (subst new old (cdr lat)))))))

(define multirember
   (lambda (a lat)
      (cond
         ((null? lat) '())
         ((eq? a (car lat)) (multirember a (cdr lat)))
         (else
          (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
   (lambda (new old lat)
      (cond 
         ((null? lat) '())
         ((eq? old (car lat))
          (cons old (cons new (multiinsertR new old (cdr lat)))))
         (else
          (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define add1
   (lambda (n)
      (+ n 1)))

(define sub1
   (lambda (n)
      (- n 1)))


(define addtup
   (lambda (tup)
      (cond
         ((null? tup) 0)
         (else
          (+ (car tup) (addtup (cdr tup)))))))

(define tup+
   (lambda (tup1 tup2)
     (cond
      ((and (null? tup1) (null? tup2)) '())
      (else
       (cons (+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(define rember*
   (lambda (element lst)
      (cond
         ((null? lst) '())
         ((list? (car lst))
          (cons (rember* element (car lst))
                (rember* element (cdr lst))))
         ((and (atom? (car lst)) (eq? element (car lst)))
          (rember* element (cdr lst)))
         (else
          (cons (car lst)
                (rember* element (cdr lst)))))))

(define insertR*
   (lambda (new old l)
      (cond
         ((null? l) '())
         ((list? (car l))
          (cons (insertR* new old (car l))
                (insertR* new old (cdr l))))
         ((and (atom? (car l))
               (eq? old (car l)))
          (cons old (cons new (insertR* new old (cdr l)))))
         (else
          (cons (car l) (insertR* new old (cdr l)))))))

(define insertL*
   (lambda (new old l)
      (cond
         ((null? l) '())
         ((list? (car l))
          (cons (insertL* new old (car l))
                (insertL* new old (cdr l))))
         ((and (atom? (car l))
               (eq? old (car l)))
          (cons new (cons old (insertL* new old (cdr l)))))
         (else
          (cons (car l) (insertL* new old (cdr l)))))))



(define occur*
   (lambda (a l)
      (cond 
         ((null? l) 0)
         ((list? (car l))
          (+ (occur* a (car l))
             (occur* a (cdr l))))
         ((eq? (car l) a)
          (+ 1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)))))) 

(define subst*
   (lambda (new old l)
      (cond
         ((null? l) '())
         ((list? (car l))
          (cons (subst* new old (car l))
                (subst* new old (cdr l))))
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
          (cons (car l) (subst* new old (cdr l)))))))


(define member*
   (lambda (a l)
      (cond
         ((null? l) #f)
         ((list? (car l))
          (or (member* a (car l))
              (member* a (cdr l))))
         ((eq? (car l) a) #t)
         (else
          (member* a (cdr l))))))  

(define leftmost
   (lambda (l)
      (cond
         ((list? (car l)) (leftmost (car l)))
         (else
          (car l)))))

(define eqlist? 
   (lambda (l1 l2)
      (cond
         ((and (null? l1) (null? l2)) #t)
         ((and (list? (car l1)) (list? (car l2)))
          (and (eqlist? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))))
         ((eq? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))
         (else
          #f))))

(define numbered?
   (lambda (exp)
      (cond
         ((atom? exp) (number? exp))
         (else
          (and (numbered? (car exp))
               (numbered? (car (cdr (cdr exp)))))))))

(define value
   (lambda (nexp)
    (cond
      ((number? nexp) nexp)
      ((eq? (car nexp) '+)
       (+ (value (car (cdr nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp)) '^)
       (expt (value (cdr nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp)) '*)
       (* (value (cdr nexp))
          (value (car (cdr (cdr nexp))))))))
 
 
(define zero?
   (lambda (n)
      (null? n)))

(define add1
   (lambda (n)
      (cons '() n)))

(define sub1
   (lambda (n)
      (cdr n)))

(define set?
   (lambda (s)
      (cond
         ((null? s) #t)
         ((member? (car s) (cdr s)) #f)
         (else
          (set? (cdr s))))))

(define makeset
   (lambda (e)
     (cond
       ((null? e) '())
       ((member? (car e) (cdr e))
        (makeset (cdr e)))
       (else
        (cons (car e) (makeset (cdr e)))))))

(define subset?
   (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      ((member? (car s1) s2)
       (subset? (cdr s1) s2))
      (else
       #f))))

(define eqset?
   (lambda (s1 s2)
      (and (subset? s1 s2) 
           (subset? s2 s1))))

(define intersect?
   (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else
       (intersect? (cdr set1) set2)))))

(define intersect
   (lambda (set1 set2)
      (cond
       ((null? set1) '())
       ((member? (car set1) set2)
        (cons (car set1)
              (intersect (cdr set1) set2)))
       (else
        (intersect (cdr set1) set2)))))

(define union
   (lambda (s1 s2)
      ((null? s1) s2)
      ((member? (car s1) s2)
       (union (cdr s1) s2))
      (else
       (cons (car s1)
             (union (cdr s1) s2)))))

(define intersectall
   (lambda (l-set)
      (cond
         ((null? (cdr l-set)) (car l-set))
         (else
          (intersect (car l-set)
            (intersectall (cdr l-set)))))))

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
      (car p)))

(define second
   (lambda (p)
      (car (cdr p))))

(define build
   (lambda (s1 s2)
      (cons s1 (cons s2 '()))))

(define third
   (lambda (l)
      (car (cdr (cdr l)))))

(define fun?
   (lambda (rel)
      (set? firsts rel)))

(define revrel
   (lambda (rel)
      (cond
         ((null? rel) '())
         (else (cons (build (second (car rel))
                            (first (car rel)))
                  (revrel (cdr rel)))))))

(define fullfun?
   (lambda (fun)
      (set? (seconds fun))))

(define rember-f
   (lambda (test?)
      (lambda (a l)
         (cond
           ((null? l) '())
           ((test? (car l) a) (cdr l))
           (else (cons (car l) 
                       ((rember-f test?) a 
                                         (cdr l))))))))

(define insertL-f
  (lambda (test?)
     (lambda (new old l)
       (cond
         ((null? l) '())
         ((test? (car l) old)
          (cons new (cons old (cdr l))))
         (else (cons (car l)
                     ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
     (lambda (new old l)
       (cond
         ((null? l) '())
         ((test? (car l) old)
          (cons old (cons new (cdr l))))
         (else (cons (car l)
                     ((insertR-f test?) new old (cdr l))))))))

(define seqL 
   (lambda (new old l)
      (cons new (cons old l))))

(define seqR
   (lambda (new old l)
      (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

(define insertL 
   (insert-g 
      (lambda (new old l)
         (cons new (cons old l)))))

(define insertR
   (insert-g 
      (lambda (new old l)
         (cons old (cons old l)))))

(define seqS
  (lambda (new old l)
     (cons new l)))

(define subst (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond 
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else expt))))

(define value
  (lambda (nexp)
     (cond
       ((atom? nexp) nexp)
       (else
        ((atom-to-function
             (operator nexp))
         (value (first-sub-exp nexp))
         (value (second-sub-exp nexp)))))))

(define mulirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat))
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                ((multirember-f test?) a)
                    (cdr lat)))))))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat)
              (muliremberT test? (cdr lat)))))))

(define new-friend
  (lambda (newlat seen)
    (col newlat
      (cons 'tuna seen))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair))
        (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
             (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
       (+ (* (weight* (first pora)) 2)
          (weight* (second pora)))))))


(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
              (A n (sub1 m)))))))

;will-stop? is an attempt at a famous halting problem.
;it cannot be written.

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
      (first entry)
      (second entry)
      entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else (lookup-in-entry-help name
               (cdr names)
               (cdr values)
               entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name
              (car table)
              (lambda (name)
                (lookup-in-table name
                  (cdr table)
                  table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    ((number? e) *const)
    ((eq? e #t) *const)
    ((eq? e #f) *const)
    ((eq? e 'cons) *const)
    ((eq? e 'car) *const)
    ((eq? e 'cdr) *const)
    ((eq? e 'null?) *const)
    ((eq? e 'eq?) *const)
    ((eq? e 'atom?) *const)
    ((eq? e 'zero?) *const)
    ((eq? e 'add1) *const)
    ((eq? e 'sub1) *const)
    ((eq? e 'number?) *const)
    (else *identifier)))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
        ((eq? (car e) 'quote)
         *quote)
        ((eq? (car e) 'lambda)
         *lambda)
        ((eq? (car e) 'cond)
         *cond)
        (else *application)))
      (else *application))))

(define value
   (lambda (e)
     (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
      (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines))
               table))
     ((meaning (question-of (car lines))
               table)
      (meaning (answer-of (car lines))
               table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
       (cons (meaning (car args) table)
             (evlist (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
      (meaning (function-of e) table)
      (evlist (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))


