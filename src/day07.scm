(require-extension utils)

; update states to be strings
(define (parse l)
  (let loop ((buffer '()) (head l) (state 0) (result '()))
    (cond
      ((and (= state 3) (null? head))
       (append result (list buffer)))
      ((null? head)
       result)
      ((and (= state 0) (equal? (car head) #\())
       (loop '() (cdr head) 1 (append result (list buffer))))
      ((and (= state 1) (equal? (car head) #\)))
       (loop '() (cdr head) 2 (append result (list buffer))))
      ((and (= state 2) (equal? (car head) #\>))
       (loop '() (cddr head) 3 result))
      ((and (= state 3) (equal? (car head) #\space))
       (loop '() (cdr head) 3 (append result (list buffer))))
      (else
        (loop (append buffer (list (car head))) (cdr head) state result)))))

(print (parse (string->list "test(10) -> test test")))
