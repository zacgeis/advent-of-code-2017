(require-extension utils)
(require-extension srfi-1)
(require-extension srfi-13)

(define (vector-swap! vec i j)
  (let ((t (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j t)))

(define (vector-shift! vec)
  (let ((last (vector-ref vec (- (vector-length vec) 1))))
    (let loop ((n 0) (t last))
      (if (< n (vector-length vec))
        (let ((vn (vector-ref vec n)))
          (vector-set! vec n t)
          (loop (+ n 1) vn))))))

(define (vector-find vec v)
  (let loop ((n 0))
    (if (< n (vector-length vec))
      (if (equal? (vector-ref vec n) v) n
        (loop (+ n 1))) -1)))

(define (vector-spin! vec n)
  (if (> n 0)
    (begin
      (vector-shift! vec)
      (vector-spin! vec (- n 1)))))

(define (parse-action s)
  (let ((action (string-take s 1)))
    (cond
      ((equal? action "s") (list action (string->number (string-drop s 1))))
      ((equal? action "p") (append (list action) (string-split (string-drop s 1) "/")))
      ((equal? action "x") (append (list action) (map string->number (string-split (string-drop s 1) "/")))))))

(define actions (map parse-action (string-split (car (string-split (read-all "data/day16-1.txt") "\n")) ",")))

(define (compile vec acts)
  (if (null? acts) '()
    (let ((act (car acts)))
      (cond
        ((equal? (car act) "s")
         (cons (list vector-spin! vec (cadr act)) (compile vec (cdr acts))))
        ((equal? (car act) "p")
         (cons (list vector-swap! vec (list vector-find vec (cadr act)) (list vector-find vec (caddr act))) (compile vec (cdr acts))))
        ((equal? (car act) "x")
         (cons (list vector-swap! vec (cadr act) (caddr act)) (compile vec (cdr acts))))))))

(define programs #("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"))
(define compiled-actions (compile programs actions))
(define (repeat-eval compiled-lst n)
  (if (> n 0)
    (begin
      (for-each eval compiled-lst)
      (cons (foldr string-append "" (vector->list programs)) (repeat-eval compiled-lst (- n 1))))
    '()))
(define res (list->vector (repeat-eval compiled-actions 60)))
(print (vector-ref res 0))
(print (vector-ref res (remainder (- 1000000000 1) 60)))

; (define xi 0)
; (define xj 0)
; (for-each
;   (lambda (i)
;     (for-each
;       (lambda (j) (if (and (equal? i j) (not (equal? xi xj))) (print i " " j " " xi " " xj)) (set! xj (+ xj 1))) res)
;     (set! xi (+ xi 1))
;     (set! xj 0)) res)
