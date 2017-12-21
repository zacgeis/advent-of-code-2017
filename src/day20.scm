(require-extension utils)
(require-extension srfi-1)
(require-extension srfi-69)

(define input
  (map (lambda (s)
         (map (lambda(s1) (map string->number (cdr (string-split s1 "<,>=")))) (string-split s " ")))
    (string-split (read-all "data/day20-1.txt") "\n")))

(define (distance p)
  (+ (abs (first (car p))) (abs (second (car p))) (abs (third (car p)))))

(define (tick p n)
  (let* ((update (map (lambda (x) (- (* 0.5 (+ n 1) (+ (* n (cadr x)) (* 2 (car x)))) (car x))) (zip (second p) (third p)))))
    (list (map (lambda (x) (apply + x)) (zip update (first p))) (second p) (third p))))

(define (step p)
  (let* ((vel (map (lambda (x) (apply + x)) (zip (second p) (third p))))
         (pos (map (lambda (x) (apply + x)) (zip (first p) vel))))
    (list pos vel (third p))))

(define (min-distance p1 p2)
  (if (> (distance p1) (distance p2)) p2 p1))

(define (min-pos ps)
  (let loop ((i 0) (m 0) (md -1) (head ps))
    (cond
      ((null? head) m)
      ((= md -1) (loop (+ i 1) 0 (distance (car head)) (cdr head)))
      ((< (distance (car head)) md) (loop (+ i 1) i (distance (car head)) (cdr head)))
      (else (loop (+ i 1) m md (cdr head))))))

(print (min-pos (map (lambda (p) (tick p 1000)) input)))

(define (check-collisions ps)
  (let ((ht (make-hash-table)))
    (let loop ((head ps))
      (if (null? head) #f
        (if (hash-table-exists? ht (caar head)) #t
          (begin
            (hash-table-set! ht (caar head) #t)
            (loop (cdr head))))))))

(define (remove-collisions ps)
  (if (equal? (check-collisions ps) #f) ps
    (let loop ((visited '()) (head ps) (res '()))
      (cond
        ((null? head) res)
        ((member (caar head) visited) (loop visited (cdr head) (delete (assoc (caar head) res) res)))
        (else (loop (cons (caar head) visited) (cdr head) (cons (car head) res)))))))

(define (find-stable-state ps)
  (let loop ((st ps) (i 0))
    (print i " " (length st))
    (let ((new-st (remove-collisions (map (lambda (p) (step p)) st))))
      (loop new-st (+ i 1)))))

(find-stable-state input)
