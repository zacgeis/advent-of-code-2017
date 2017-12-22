(require-extension utils)
(require-extension srfi-1)

;'((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))
; (define input '((1 2) (4 3)))
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
    (map reverse (apply zip input))
    (reverse (apply zip input))))

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
  (print-g gl)
  (caar gl))

(define (process g rules)
  (let loop ((i 0) (grid g))
    (cond
      ((= i 2) grid)
      (else (loop (+ i 1) (combine (map (lambda (gs) (cdr (find-rule rules gs))) (split (if (even? (length grid)) 2 3) grid))))))))

(define rules
  (map (lambda (s)
         (map (lambda (s1) (map (lambda (s2) (string->list s2)) (string-split s1 "/")))
              (string-split s " => ")))
       (string-split (read-all "data/day21-1.txt") "\n")))

(define input '((#\. #\# #\.) (#\. #\. #\#) (#\# #\# #\#)))
(print (process input rules))
; (print (split 2 (cadr (find-rule rules input))))

(print (split 2 '((1 2 3 4 0 0) (5 6 7 8 0 0) (9 10 11 12 0 0) (13 14 15 16 0 0) (1 1 1 1 1 1) (1 1 1 1 1 1))))

