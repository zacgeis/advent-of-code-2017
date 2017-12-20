(require-extension utils)
(require-extension srfi-1)

(define (vector-find vec v)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (if (equal? (vector-ref vec i) v) i
        (loop (+ i 1))) -1)))

(define (ref2d vec pos)
  (vector-ref (vector-ref vec (cadr pos)) (car pos)))

(define (step pos vel)
  (map (lambda (p) (apply + p)) (zip pos vel)))

(define (turn vec pos vel)
  (let* ((dirs '((-1 0) (1 0) (0 -1) (0 1)))
         (filtered-dirs (delete (map (lambda (x) (* x -1)) vel) dirs)))
    (car (filter (lambda (v) (not (equal? (ref2d vec (step pos v)) #\space))) filtered-dirs))))

(define (follow-track vec start-pos)
  (let loop ((pos start-pos) (vel '(0 1)) (s 0) (word ""))
    (let ((char (ref2d vec pos)))
      (cond
        ((equal? char #\space) (+ s 1) (list s word))
        ((or (equal? char #\-) (equal? char #\|)) (loop (step pos vel) vel (+ s 1) word))
        ((equal? char #\+)
         (let ((new-vel (turn vec pos vel)))
           (loop (step pos new-vel) new-vel (+ s 1) word)))
        (else (loop (step pos vel) vel (+ s 1) (string-append word (list->string (list char)))))))))

(define input
  (list->vector
    (map (lambda (s) (list->vector (string->list s)))
      (string-split (read-all "data/day19-1.txt") "\n"))))

(define start-pos (list (vector-find (vector-ref input 0) #\|) 0))

(print (follow-track input start-pos))
