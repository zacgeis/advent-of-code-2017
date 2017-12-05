(require-extension utils)
(require-extension srfi-1)

(define (read-as-vector path)
  (list->vector (map string->number (string-split (read-all path) "\n"))))

(define (problem-1 path)
  (let loop ((v (read-as-vector path)) (i 0) (c 0))
    (cond
      ((or (>= i (vector-length v)) (< i 0)) c)
      (else (let ((j (vector-ref v i)))
              (loop (begin (vector-set! v i (+ j 1)) v) (+ i j) (+ c 1)))))))

(define (problem-2 path)
  (let loop ((v (read-as-vector path)) (i 0) (c 0))
    (cond
      ((or (>= i (vector-length v)) (< i 0)) c)
      (else (let ((j (vector-ref v i)))
              (loop (begin (vector-set! v i (if (>= j 3) (- j 1) (+ j 1))) v) (+ i j) (+ c 1)))))))

(print (problem-1 "data/day05-1.txt"))
(print (problem-2 "data/day05-1.txt"))
