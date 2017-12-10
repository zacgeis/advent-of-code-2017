(require-extension utils)
(require-extension srfi-1)

(define (vector-swap! vec i j)
  (let ((t (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j t)))

(define (vector-wrapped-reverse! vec i n)
  (let loop ((swaps (inexact->exact (floor (/ n 2)))))
    (if (> swaps 0)
      (let* ((len (vector-length vec))
             (s1 (- (+ i swaps) 1))
             (s2 (- (+ i n) swaps)))
        (vector-swap! vec (remainder s1 len) (remainder s2 len))
        (loop (- swaps 1))))))

(define (range num)
  (let ((vec (make-vector num)))
    (let loop ((i 0))
      (if (= i num)
        vec
        (begin (vector-set! vec i i) (loop (+ i 1)))))))

(define (knot-hash! v lens iterations)
  (let loop ((head lens) (i 0) (skip 0) (iteration iterations))
    (if (> iteration 0)
      (begin
        (vector-wrapped-reverse! v i (car head))
        (let ((new-i (remainder (+ i (car head) skip) (vector-length v)))
              (new-skip (+ skip 1)))
          (cond
            ((null? (cdr head)) (loop lens new-i new-skip (- iteration 1)))
            (else (loop (cdr head) new-i new-skip iteration))))))))

(define input-1 '(147 37 249 1 31 2 226 0 161 71 254 243 183 255 30 70))
(define problem-1-vec (range 256))
(knot-hash! problem-1-vec input-1 1)
(print (* (vector-ref problem-1-vec 0) (vector-ref problem-1-vec 1)))

(define (sections l n)
  (let loop ((head l) (cur n) (buffer '()) (result '()))
    (cond
      ((= cur 0) (loop head n '() (append result (list buffer))))
      ((null? head) result)
      (else (loop (cdr head) (- cur 1) (append buffer (list (car head))) result)))))

(define (integer->stringhex i)
  (let ((lookup (list->vector (string->list "0123456789abcdef"))))
    (if (> i 255)
      (string-append (integer->stringhex (- i 255)) (integer->stringhex 255))
      (list->string
        (list
          (vector-ref lookup (inexact->exact (floor (/ i 16))))
          (vector-ref lookup (remainder i 16)))))))

(define input-2
  (append
    (map char->integer (string->list "147,37,249,1,31,2,226,0,161,71,254,243,183,255,30,70"))
    '(17 31 73 47 23)))
(define problem-2-vec (range 256))
(knot-hash! problem-2-vec input-2 64)
(print
  (foldr string-append ""
    (map (lambda (section) (integer->stringhex (fold bitwise-xor 0 section)))
         (sections (vector->list problem-2-vec) 16))))
