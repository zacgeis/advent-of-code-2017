(require-extension utils)
(require-extension srfi-1)
(require-extension srfi-69)

; (define (sparse-grid-set g p v)
;   (cond
;     ((equal? v #\.) (delete (assoc p g) g))
;     ((assoc p g) (cons (list p v) (delete (assoc p g) g)))
;     (else (cons (list p v) g))))
;
; (define (sparse-grid-get g p)
;   (let ((v (assoc p g)))
;     (if v (cadr v) #\.)))
;
; (define (make-sparse-grid in)
;   (let loop ((y (- (vector-length in) 1))
;              (x (- (vector-length (vector-ref in 0)) 1))
;              (g '()))
;     (cond
;       ((< y 0) g)
;       ((< x 0) (loop (- y 1) (- (vector-length (vector-ref in 0)) 1) g))
;       (else (loop y (- x 1) (sparse-grid-set g (list x y) (vector-ref (vector-ref in y) x)))))))

(define (sparse-grid-set g p v)
  (let ((ex (hash-table-exists? g p)))
    (cond
      ((and (equal? v #\.) ex) (hash-table-delete! g p) g)
      ((equal? v #\.) g)
      (else (hash-table-set! g p v) g))))

(define (sparse-grid-get g p)
  (let ((ex (hash-table-exists? g p)))
    (if ex (hash-table-ref g p) #\.)))

(define (make-sparse-grid in)
  (let loop ((y (- (vector-length in) 1))
             (x (- (vector-length (vector-ref in 0)) 1))
             (g (make-hash-table)))
    (cond
      ((< y 0) g)
      ((< x 0) (loop (- y 1) (- (vector-length (vector-ref in 0)) 1) g))
      (else (loop y (- x 1) (sparse-grid-set g (list x y) (vector-ref (vector-ref in y) x)))))))

(define (turn-left d)
  (cond
    ((equal? d '(0 -1)) '(-1 0))
    ((equal? d '(-1 0)) '(0 1))
    ((equal? d '(0 1)) '(1 0))
    ((equal? d '(1 0)) '(0 -1))))

(define (turn-right d)
  (cond
    ((equal? d '(0 -1)) '(1 0))
    ((equal? d '(1 0)) '(0 1))
    ((equal? d '(0 1)) '(-1 0))
    ((equal? d '(-1 0)) '(0 -1))))

(define (problem-1 in n)
  (let loop ((sg (make-sparse-grid in))
             (i 0)
             (c 0)
             (p (list
                  (- (inexact->exact (ceiling (/ (vector-length (vector-ref in 0)) 2))) 1)
                  (- (inexact->exact (ceiling (/ (vector-length in) 2))) 1)))
             (d '(0 -1)))
    (let* ((state (sparse-grid-get sg p))
           (new-d (if (equal? state #\#) (turn-right d) (turn-left d)))
           (new-s (if (equal? state #\#) #\. #\#))
           (new-c (if (equal? new-s #\#) (+ c 1) c)))
      (if (= i n) c
        (loop (sparse-grid-set sg p new-s) (+ i 1) new-c (map (lambda (x) (apply + x)) (zip p new-d)) new-d)))))

(define (state-change s)
  (cond
    ((equal? s #\.) #\w)
    ((equal? s #\w) #\#)
    ((equal? s #\#) #\f)
    ((equal? s #\f) #\.)))

(define (direction-change s d)
  (cond
    ((equal? s #\.) (turn-left d))
    ((equal? s #\w) d)
    ((equal? s #\#) (turn-right d))
    ((equal? s #\f) (turn-right (turn-right d)))))

(define (problem-2 in n)
  (let loop ((sg (make-sparse-grid in))
             (i 0)
             (c 0)
             (p (list
                  (- (inexact->exact (ceiling (/ (vector-length (vector-ref in 0)) 2))) 1)
                  (- (inexact->exact (ceiling (/ (vector-length in) 2))) 1)))
             (d '(0 -1)))
    (let* ((state (sparse-grid-get sg p))
           (new-d (direction-change state d))
           (new-s (state-change state))
           (new-c (if (equal? new-s #\#) (+ c 1) c)))
      (if (= i n) c
        (loop (sparse-grid-set sg p new-s) (+ i 1) new-c (map (lambda (x) (apply + x)) (zip p new-d)) new-d)))))

(define input
  (list->vector
    (map (lambda (s) (list->vector (string->list s)))
       (string-split (read-all "data/day22-1.txt") "\n"))))

(print (problem-1 input 10000))
(print (problem-2 input 10000000))
