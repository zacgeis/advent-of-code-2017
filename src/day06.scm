(require-extension utils)
(require-extension srfi-1)

(define (distribute lst start)
  (let loop ((steps 0) (distance-remaining -1) (org-lst lst) (new-lst '()))
    (cond
      ((and (<= distance-remaining 0) (null? org-lst)) new-lst)
      ((null? org-lst)
       (loop steps distance-remaining new-lst '()))
      ((= steps start)
       (loop (+ steps 1) (car org-lst) (cdr org-lst) (append new-lst '(0))))
      ((and (>= steps start) (> distance-remaining 0))
       (loop (+ steps 1) (- distance-remaining 1) (cdr org-lst) (append new-lst (list (+ (car org-lst) 1)))))
      (else
        (loop (+ steps 1) distance-remaining (cdr org-lst) (append new-lst (list (car org-lst))))))))

(define (find-max-pos lst)
  (let loop ((t lst) (i 0) (pos 0) (val (car lst)))
    (cond
      ((null? t) pos)
      ((> (car t) val) (loop (cdr t) (+ i 1) i (car t)))
      (else (loop (cdr t) (+ i 1) pos val)))))

(define (problem-1 l)
  (let loop ((steps 0) (current l) (previous '()))
    (if (member current previous) steps
      (let ((next (distribute current (find-max-pos current))))
        (loop (+ steps 1) next (cons current previous))))))

(define (problem-2 l)
  (let loop ((steps 0) (current l) (previous '()))
    (let ((dup (find (lambda (p) (equal? current (cadr p))) previous)))
      (if dup (- steps (car dup))
        (let ((next (distribute current (find-max-pos current))))
          (loop (+ steps 1) next (cons (list steps current) previous)))))))

(define input (list 4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3))
(print (problem-1 input))
(print (problem-2 input))
