(require-extension utils)
(require-extension srfi-1)

(define (parse-arg s)
  (let ((n (string->number s)))
    (if (equal? n #f) s n)))

(define (parse-action s)
  (let* ((l (string-split s " ")))
    (append (list (car l)) (map parse-arg (cdr l)))))

(define input
  (map parse-action
    (string-split (read-all "data/day18-1.txt") "\n")))

(define (read-reg regs r)
  (let ((l (assoc r regs)))
    (if l (cadr l) 0)))

(define (write-reg regs r v)
  (if (equal? (assoc r regs) #f)
    (cons (list r v) regs)
    (let loop ((head regs) (new-regs '()))
      (cond
        ((null? head) new-regs)
        ((equal? (caar head) r) (loop (cdr head) (cons (list r v) new-regs)))
        (else (loop (cdr head) (cons (list (caar head) (cadar head)) new-regs)))))))

(define (prepare-args lst regs)
  (map (lambda (o) (if (number? o) o (read-reg regs o))) lst))

(define (execute vec)
  (let loop ((pc 0) (regs '()) (freq '()))
    (let* ((command (vector-ref vec pc))
           (op (car command))
           (prepared-args (prepare-args (cdr command) regs)))
      (cond
        ((and (equal? op "rcv") (not (equal? freq 0))) freq)
        ((equal? op "snd") (loop (+ pc 1) regs (car prepared-args)))
        ((equal? op "set") (loop (+ pc 1) (write-reg regs (cadr command) (cadr prepared-args)) freq))
        ((equal? op "add") (loop (+ pc 1) (write-reg regs (cadr command) (+ (read-reg regs (cadr command)) (cadr prepared-args))) freq))
        ((equal? op "mul") (loop (+ pc 1) (write-reg regs (cadr command) (* (read-reg regs (cadr command)) (cadr prepared-args))) freq))
        ((equal? op "mod") (loop (+ pc 1) (write-reg regs (cadr command) (remainder (read-reg regs (cadr command)) (cadr prepared-args))) freq))
        ((and (equal? op "jgz") (> (car prepared-args) 0)) (loop (+ pc (cadr prepared-args)) regs freq))
        (else (loop (+ pc 1) regs freq))))))

(print (execute (list->vector input)))

(define (step vec pc regs in out c)
  (let* ((command (vector-ref vec pc))
         (op (car command))
         (prepared-args (prepare-args (cdr command) regs)))
    (print (assoc "prg" regs) " " c)
    (cond
      ((and (equal? op "rcv"))
       (if (null? in)
         (list vec pc regs in out c)
         (list vec (+ pc 1) (write-reg regs (cadr command) (car in)) (cdr in) out c)))
      ((equal? op "snd") (list vec (+ pc 1) regs in (append out (list (car prepared-args))) (+ c 1)))
      ((equal? op "set") (list vec (+ pc 1) (write-reg regs (cadr command) (cadr prepared-args)) in out c))
      ((equal? op "add") (list vec (+ pc 1) (write-reg regs (cadr command) (+ (read-reg regs (cadr command)) (cadr prepared-args))) in out c))
      ((equal? op "mul") (list vec (+ pc 1) (write-reg regs (cadr command) (* (read-reg regs (cadr command)) (cadr prepared-args))) in out c))
      ((equal? op "mod") (list vec (+ pc 1) (write-reg regs (cadr command) (remainder (read-reg regs (cadr command)) (cadr prepared-args))) in out c))
      ((and (equal? op "jgz") (> (car prepared-args) 0)) (list vec (+ pc (cadr prepared-args)) regs in out c))
      (else (list vec (+ pc 1) regs in out c)))))

(define (run-parallel prog1 prog2)
  (let loop ((i 0) (prog1 prog1) (prog2 prog2))
    (let* ((prog1-vec (first prog1))
           (prog1-pc (second prog1))
           (prog1-regs (third prog1))
           (prog1-c (sixth prog1))
           (prog2-vec (first prog2))
           (prog2-pc (second prog2))
           (prog2-regs (third prog2))
           (prog2-in (fourth prog2))
           (prog2-out (fifth prog2))
           (prog2-c (sixth prog2))
           (new-prog1 (apply step (list prog1-vec prog1-pc prog1-regs prog2-out prog2-in prog1-c)))
           (new-prog1-in (fourth new-prog1))
           (new-prog1-out (fifth new-prog1)))
      (if (>= i 0)
        (loop (+ i 1)
              new-prog1
              (apply step (list prog2-vec prog2-pc prog2-regs new-prog1-out new-prog1-in prog2-c)))))))

(define prog1 (list (list->vector input) 0 '(("prg" 0) ("p" 0)) '() '() 0))
(define prog2 (list (list->vector input) 0 '(("prg" 1) ("p" 1)) '() '() 0))
(run-parallel prog1 prog2)
