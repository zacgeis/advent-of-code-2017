(require-extension utils)
(require-extension srfi-1)
(require-extension srfi-69)

(define (search f)
  (if (f 1 '(0 0))
    '(1 (0 0))
    (let loop
      ((current 2) (pos '(1 0)) (steps 1) (side 2) (direction #\n))
      (let ((new-current (+ current 1)) (new-pos ((direction-transform direction) pos)))
        (cond
          ((f current pos) (list current pos))
          ((and (= steps 1) (eq? direction #\e)) (loop new-current new-pos (- (+ side 2) 1) (+ side 2) (turn direction)))
          ((and (= steps 1) (eq? direction #\s)) (loop new-current new-pos (+ side 1) side (turn direction)))
          ((= steps 1) (loop new-current new-pos side side (turn direction)))
          (else (loop new-current new-pos (- steps 1) side direction)))))))

(define (turn d)
  (case d
    ((#\n) #\w)
    ((#\w) #\s)
    ((#\s) #\e)
    ((#\e) #\n)))

(define (direction-transform d)
  (case d
    ((#\n) (lambda (p) (list (car p) (+ (cadr p) 1))))
    ((#\e) (lambda (p) (list (+ (car p) 1) (cadr p))))
    ((#\s) (lambda (p) (list (car p) (- (cadr p) 1))))
    ((#\w) (lambda (p) (list (- (car p) 1) (cadr p))))))

(define (problem-1 num)
  (let ((pos (cadr (search (lambda (c p) (= c num))))))
    (+ (abs (car pos)) (abs (cadr pos)))))

(define (memoize f)
  (let ((h (make-hash-table)))
    (lambda (x . xs)
      (let ((args (cons x xs)))
        (if (hash-table-exists? h args)
          (hash-table-ref h args)
          (begin
            (let ((r (apply f args)))
              (hash-table-set! h args r) r)))))))

(define (sum-adj num)
  (if (= num 1) 1
    (let ((org (cadr (search (lambda (c p) (= c num))))))
      (let
        ((adj-list (list
          (search (lambda (c p) (equal? p ((direction-transform #\n) org))))
          (search (lambda (c p) (equal? p ((direction-transform #\e) org))))
          (search (lambda (c p) (equal? p ((direction-transform #\s) org))))
          (search (lambda (c p) (equal? p ((direction-transform #\w) org))))
          (search (lambda (c p) (equal? p ((direction-transform #\n) ((direction-transform #\w) org)))))
          (search (lambda (c p) (equal? p ((direction-transform #\s) ((direction-transform #\w) org)))))
          (search (lambda (c p) (equal? p ((direction-transform #\n) ((direction-transform #\e) org)))))
          (search (lambda (c p) (equal? p ((direction-transform #\s) ((direction-transform #\e) org))))))))
        (fold (lambda (r a) (+ (sum-adj (car r)) a)) 0 (filter (lambda (r) (< (car r) num)) adj-list))))))
(define sum-adj (memoize sum-adj))

(define (problem-2 num)
  (let loop ((current 1))
    (let ((result (sum-adj current)))
      (if (< result num) (loop (+ current 1)) result))))

(print (problem-1 289326))
(print (problem-2 289326))
