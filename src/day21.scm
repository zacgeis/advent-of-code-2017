(require-extension utils)
(require-extension srfi-1)

(define (print-g g)
  (for-each print g)
  (newline))

(define (rotations l)
  (list l
    (map reverse l)
    (reverse (map reverse l))
    (reverse l)))

(define (flips l)
  (list l
    (reverse (map reverse l))
    (map reverse (apply zip l))
    (reverse (apply zip l))))

(define (flip-rotations l)
  (foldr append '() (map rotations (flips l))))

(define (sections n l)
  (if (null? l) '()
    (cons (take l n) (sections n (drop l n)))))

(define (split n g)
  (foldr append '()
         (map (lambda (l) (apply zip l))
              (sections n (map (lambda (l) (sections n l)) g)))))

(define (find-rule rules g)
  (any (lambda (l) (assoc l rules)) (flip-rotations g)))

(define (combine gl)
  (let ((n (inexact->exact (sqrt (length gl)))))
    (let loop ((grid gl) (r '()))
      (if (null? grid) (map (lambda (l) (apply append l)) r)
        (loop (drop grid n) (append r (apply zip (take grid n))))))))

(define (process g rules n)
  (let loop ((i 0) (grid g))
    (cond
      ((= i n) grid)
      (else (loop (+ i 1)
                  (combine (map (lambda (gs) (cadr (find-rule rules gs)))
                                (split (if (even? (length grid)) 2 3) grid))))))))

(define (count-cells g)
  (fold + 0
    (map (lambda (r) (fold (lambda (c a) (if (equal? c #\#) (+ a 1) a)) 0 r)) g)))

(define rules
  (map (lambda (s)
         (map (lambda (s1) (map (lambda (s2) (string->list s2)) (string-split s1 "/")))
              (string-split s " => ")))
       (string-split (read-all "data/day21-1.txt") "\n")))

(define input '((#\. #\# #\.) (#\. #\. #\#) (#\# #\# #\#)))
(print (count-cells (process input rules 5)))
(print (count-cells (process input rules 18)))

