(require-extension utils)
(require-extension srfi-1)

(define input
  (map
    (lambda (s) (map string->number (string-split s "/")))
    (string-split (read-all "data/day24-1.txt") "\n")))

(define (max-strength p1 p2)
  (if (> (cadr p1) (cadr p2)) p1 p2))

(define (max-length p1 p2)
  (cond
    ((= (car p1) (car p2)) (max-strength p1 p2))
    ((> (car p1) (car p2)) p1)
    (else p2)))

(define (find-max l)
  (let loop ((current 0) (rem l) (str 0) (len 0))
    (let ((fits (filter (lambda (p) (member current p)) rem)))
      (cond
        ((null? fits) (list len (+ str current)))
        (else
          (fold max-length '(0 0)
            (map (lambda (p)
                   (loop
                     (car (append (delete current p) (list current)))
                     (delete p rem)
                     (+ str (* 2 current))
                     (+ len 1)))
                 fits)))))))

(print (find-max input))

