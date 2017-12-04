(require-extension utils)
(require-extension srfi-1)

(define (duplicate? l)
  (let loop ((left '()) (mid (car l)) (right (cdr l)))
    (cond
      ((or (member mid left) (member mid right)) #t)
      ((null? (cdr right)) #f)
      (else (loop (cons mid left) (car right) (cdr right))))))

(define (count-lines path f)
  (let ((contents (read-all path)))
    (let ((processed-lines
            (map (lambda (line) (f line))
                 (string-split contents "\n"))))
      (fold (lambda (d a) (if d (+ a 1) a)) 0 processed-lines))))

(define (problem-1 path)
  (count-lines path (lambda (line) (not (duplicate? (string-split line " "))))))

(define (problem-2 path)
  (count-lines path
    (lambda (line)
      (not (duplicate?
             (map (lambda (w) (sort (string->list w) char<?))
                  (string-split line " ")))))))

(print (problem-1 "data/day04-1.txt"))
(print (problem-2 "data/day04-2.txt"))
