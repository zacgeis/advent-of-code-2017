(require-extension utils)
(require-extension srfi-1)

(define (read-reg regs reg)
  (let ((lookup (assoc reg regs)))
    (if lookup (cadr lookup) 0)))

(define (apply-op op x y)
  (cond
    ((equal? op "inc") (+ x y))
    ((equal? op "dec") (- x y))
    (else (error "invalid op" op))))

(define (apply-op-to-reg regs reg op x)
  (if (assoc reg regs)
    (let loop ((head regs))
      (if (null? head) '()
        (cons (list (caar head)
                    (if (equal? (caar head) reg)
                      (apply-op op (cadar head) x)
                      (cadar head)))
              (loop (cdr head)))))
    (cons (list reg (apply-op op 0 x)) regs)))

(define (eval-cond regs comp-reg comp-op comp-val)
  (let ((comp-reg-val (read-reg regs comp-reg)))
    (cond
      ((equal? comp-op "<") (< comp-reg-val comp-val))
      ((equal? comp-op "<=") (<= comp-reg-val comp-val))
      ((equal? comp-op ">") (> comp-reg-val comp-val))
      ((equal? comp-op ">=") (>= comp-reg-val comp-val))
      ((equal? comp-op "==") (= comp-reg-val comp-val))
      ((equal? comp-op "!=") (not (= comp-reg-val comp-val)))
      (else (error "inavlid comp" comp-op)))))

(define (eval-statement regs reg op op-val comp-reg comp-op comp-val)
  (let ((comp-res (eval-cond regs comp-reg comp-op comp-val)))
    (if comp-res (apply-op-to-reg regs reg op op-val) regs)))

(define (solve path)
	(let ((input (map (lambda (s) (string-split s " ")) (string-split (read-all path) "\n"))))
    (let loop ((regs '()) (head input) (previous '()))
      (if (null? head) (list regs previous)
        (let* ((line (car head))
               (reg (first line))
               (op (second line))
               (op-val (string->number (third line)))
               (comp-reg (fifth line))
               (comp-op (sixth line))
               (comp-val (string->number (seventh line))))
          (loop (eval-statement regs reg op op-val comp-reg comp-op comp-val) (cdr head) (cons regs previous)))))))

(define (find-max tbl)
  (fold max 0 (map cadr tbl)))

(define (problem-1 path)
  (find-max (car (solve path))))

(define (problem-2 path)
  (fold max 0 (map find-max (cadr (solve path)))))

(print (problem-1 "data/day08-1.txt"))
(print (problem-2 "data/day08-1.txt"))
