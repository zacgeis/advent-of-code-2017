(require-extension utils)
(require-extension srfi-1)

(load "src/day10.scm")

(define (integer->bin i)
  (let loop ((p 7) (t i) (r '()))
    (let ((s (expt 2 p)))
      (if (< p 0) r
        (if (>= (- t s) 0)
          (loop (- p 1) (- t s) (append r '(1)))
          (loop (- p 1) t (append r '(0))))))))

(define (knot-hash-to-bin str)
  (let ((in (append (map char->integer (string->list str)) '(17 31 73 47 23)))
        (vec (range 256)))
    (knot-hash! vec in 64)
    (foldr append '()
      (map (lambda (section) (integer->bin (fold bitwise-xor 0 section)))
           (sections (vector->list vec) 16)))))

(define (generate-grid str)
  (let loop ((i 0) (grid '()))
    (if (> i 127) grid
      (loop (+ i 1) (append grid (list (knot-hash-to-bin (string-append str "-" (number->string i)))))))))

(define (count-grid grid)
  (fold + 0 (map (lambda (row) (fold (lambda (i a) (if (= i 1) (+ a 1) a)) 0 row)) grid)))

; (print (count-grid (generate-grid "ffayrhll")))

(define (positions len width)
  (let loop ((i 0) (pos-lst '()))
    (if (< i len)
      (loop (+ i 1) (cons (list (remainder i width) (inexact->exact (floor (/ i width)))) pos-lst))
      pos-lst)))

(define (get-neighbors pos)
  (let ((x (car pos)) (y (cadr pos)))
    (list
      (list (+ x 1) (+ y 0))
      (list (+ x 0) (+ y 1))
      (list (- x 1) (+ y 0))
      (list (+ x 0) (- y 1)))))

(define (get-pos vec width pos)
  (let* ((x (car pos))
         (y (cadr pos))
         (i (+ x (* width y))))
    (if (and (and (>= x 0) (>= y 0)) (< i (vector-length vec)))
      (vector-ref vec i)
      0)))

(define (connected vec width pos)
  (let loop ((stack (list pos)) (visited '()))
    (cond
      ((null? stack) visited)
      ((member (car stack) visited) (loop (cdr stack) visited))
      (else
        (let ((neighbors (get-neighbors (car stack))))
          (loop (append (filter (lambda (n) (= (get-pos vec width n) 1)) neighbors) (cdr stack)) (cons (car stack) visited)))))))

(define (segments width vec)
  (let loop ((visited '())
             (pos-lst (positions (vector-length vec) width))
             (count 0))
    (cond
      ((null? pos-lst) count)
      ((member (car pos-lst) visited) (loop visited (cdr pos-lst) count))
      ((= (get-pos vec width (car pos-lst)) 1)
       (let ((seg (connected vec width (car pos-lst))))
         (loop (append visited seg) (cdr pos-lst) (+ count 1))))
      (else
        (begin
          (loop (cons (car pos-lst) visited) (cdr pos-lst) count))))))

(print (segments 128 (list->vector (foldr append '() (generate-grid "ffayrhll")))))
