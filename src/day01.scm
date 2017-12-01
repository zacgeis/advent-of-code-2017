(require-extension utils)
(require-extension srfi-1)

(define (raw-input path)
  (call-with-input-file path
    (lambda (p)
      (car (string-split (read-all p) "\n")))))

(define (parsed-input r)
  (map (lambda (c) (- (char->integer c) 48))
       (string->list r)))

(define (wrap-list l)
  (append l (list (car l))))

(define (fold-with-peek f a l)
  (if (null? (cdr l)) a
    (fold-with-peek f (f a (car l) (cadr l)) (cdr l))))

(define (problem-1 l)
  (fold-with-peek (lambda (a x y) (if (= x y) (+ a x) a)) 0 (wrap-list l)))

(define (halfway-pairs l)
  (let ((mid (/ (length l) 2)))
    (zip l (append (drop l mid) (take l mid)))))

(define (problem-2 l)
  (fold (lambda (p a) (if (= (car p) (cadr p)) (+ a (car p)) a)) 0 (halfway-pairs l)))

(print (problem-1 (parsed-input (raw-input "data/day01-1.txt"))))
(print (problem-2 (parsed-input (raw-input "data/day01-2.txt"))))
