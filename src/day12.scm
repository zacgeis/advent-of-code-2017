(require-extension utils)
(require-extension srfi-1)

(define (connects-to a b visited lst)
  (let ((row (assoc a lst)))
    (cond
      ((member a visited) #f)
      ((member b row) #t)
      (else (any (lambda (c) (connects-to c b (cons a visited) lst)) (cdr row))))))

(define (problem-1 num lst)
  (let loop ((head lst) (result 0))
    (if (null? head) result
      (let ((connects (connects-to (caar head) num '() lst)))
        (loop (cdr head) (if connects (+ result 1) result))))))

(define input
  (map
    (lambda (s) (map string->number (string-split s " <->,")))
    (string-split (read-all "data/day12-1.txt") "\n")))

(print (problem-1 0 input))

(define (problem-2 lst)
  (let loop ((head lst) (result 0))
    (if (null? head) result
      (let ((new-head (filter (lambda (row) (not (connects-to (caar head) (car row) '() lst))) (cdr head))))
        (loop new-head (+ result 1))))))

(print (problem-2 input))
