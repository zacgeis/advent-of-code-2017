(require-extension utils)
(require-extension srfi-1)

(define (parse-1 l)
	(let loop ((head l) (state 0) (depth 0) (total 0))
		(cond
			((null? head) total)
			((and (equal? (car head) #\!))
			 (loop (cddr head) state depth total))
			((and (equal? (car head) #\,))
			 (loop (cdr head) state depth total))
			((and (= state 0) (equal? (car head) #\{))
			 (loop (cdr head) 0 (+ depth 1) total))
			((and (= state 0) (equal? (car head) #\}))
			 (loop (cdr head) 0 (- depth 1) (+ total depth)))
			((and (= state 0) (equal? (car head) #\<))
			 (loop (cdr head) 1 depth total))
			((and (= state 1) (equal? (car head) #\>))
			 (loop (cdr head) 0 depth total))
			(else
				(loop (cdr head) state depth total)))))

(define (parse-2 l)
	(let loop ((head l) (state 0) (total 0))
		(cond
			((null? head) total)
			((and (equal? (car head) #\!))
			 (loop (cddr head) state total))
			((and (= state 0) (equal? (car head) #\<))
			 (loop (cdr head) 1 total))
			((and (= state 1) (equal? (car head) #\>))
			 (loop (cdr head) 0 total))
      ((= state 1)
			 (loop (cdr head) state (+ total 1)))
			(else
				(loop (cdr head) state total)))))

(define (problem-1 path)
  (parse-1 (string->list (read-all path))))

(define (problem-2 path)
  (parse-2 (string->list (read-all path))))

(print (problem-1 "data/day09-1.txt"))
(print (problem-2 "data/day09-1.txt"))
