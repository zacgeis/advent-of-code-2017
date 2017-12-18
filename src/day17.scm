(require-extension utils)
(require-extension srfi-1)

(define (insert lst i v)
  (append (take lst (+ i 1)) (list v) (take-right lst (- (- (length lst) 1) i))))

(define (gen-lst n jump)
  (let loop ((i 1) (v 2) (l '(0 1)))
    (if (> v n) l
      (let ((t (remainder (+ i jump) v)))
        (loop (+ t 1) (+ v 1) (insert l t v))))))

(define (find-ind lst v)
  (let loop ((head lst) (i 0))
    (if (null? head) -1
      (if (equal? (car head) v) i (loop (cdr head) (+ i 1))))))

(define lst (gen-lst 2017 348))
(print (list-ref lst (+ (find-ind lst 2017) 1)))

(define (scoped-insert lst i v)
  (if (= i 0) (list (car lst) v) lst))

(define (scoped-gen n jump)
  (let loop ((i 1) (v 2) (l '(0 1)))
    (if (> v n) l
      (let ((t (remainder (+ i jump) v)))
        (loop (+ t 1) (+ v 1) (scoped-insert l t v))))))

(define lst (scoped-gen 50000000 348))
(print lst)
