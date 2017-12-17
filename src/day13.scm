(require-extension utils)
(require-extension srfi-1)

(define input
  (map (lambda (s)
         (map string->number (string-split s ": ")))
    (string-split (read-all "data/day13-1.txt") "\n")))

(define (input->vector lst)
  (let ((vec (make-vector (+ (car (last lst)) 1) -1)))
    (let loop ((head lst))
      (if (null? head) vec
        (begin
          (vector-set! vec (caar head) (cadar head))
          (loop (cdr head)))))))

(define (check-hit vec i s)
  (let ((limit (vector-ref vec i)))
    (and (not (= limit -1)) (= 0 (remainder s (* (- limit 1) 2))))))

(define (score-firewall vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0) (score 0))
      (if (< i len)
        (loop (+ i 1) (if (check-hit vec i i) (+ score (* (vector-ref vec i) i)) score))
        score))))

(print (score-firewall (input->vector input)))

(define (gets-caught? vec delay)
  (let ((len (vector-length vec)))
    (let loop ((time delay) (i 0))
      (if (< i len)
        (if (check-hit vec i time) #t (loop (+ time 1) (+ i 1))) #f))))

(define (find-delay vec)
  (let loop ((delay 0))
    (if (not (gets-caught? vec delay)) delay
      (loop (+ delay 1)))))

(print (find-delay (input->vector input)))
