(require-extension utils)
(require-extension srfi-1)

(define (apply-move pos dir)
  (cond
    ((equal? dir "n") (list (car pos) (+ (cadr pos) 1) (- (caddr pos) 1)))
    ((equal? dir "ne") (list (+ (car pos) 1) (cadr pos) (- (caddr pos) 1)))
    ((equal? dir "se") (list (+ (car pos) 1) (- (cadr pos) 1) (caddr pos)))
    ((equal? dir "s") (list (car pos) (- (cadr pos) 1) (+ (caddr pos) 1)))
    ((equal? dir "sw") (list (- (car pos) 1) (cadr pos) (+ (caddr pos) 1)))
    ((equal? dir "nw") (list (- (car pos) 1) (+ (cadr pos) 1) (caddr pos)))
    (else (error dir))))

(define (apply-moves pos steps)
  (if (null? steps) pos
    (apply-moves (apply-move pos (car steps)) (cdr steps))))

(define (distance-to-origin pos)
  (+ (abs (car pos)) (abs (cadr pos)) (abs (caddr pos))))

(define directions '("n" "ne" "se" "s" "sw" "nw"))

; (define (steps-to-origin pos)
;   (let loop ((queue (list (list pos 0))) (visited '()))
;     (let ((current-pos (caar queue))
;           (current-steps (cadar queue)))
;       (cond
;         ((equal? current-pos '(0 0 0)) current-steps)
;         ((member current-pos visited) (loop (cdr queue) visited))
;         (else
;           (let* ((start-distance (distance-to-origin current-pos))
;                  (new-pos (map (lambda (dir) (list (apply-move current-pos dir) (+ current-steps 1))) directions))
;                  (filtered-new-pos (filter (lambda (pos) (< (distance-to-origin (car pos)) start-distance)) new-pos)))
;             (loop
;               (append (cdr queue) filtered-new-pos)
;               (cons current-pos visited))))))))

(define (steps-to-origin pos)
  (/ (distance-to-origin pos) 2))

(define (problem-1 path)
  (let ((input (string-split (car (string-split (read-all path) "\n")) ",")))
    (steps-to-origin (apply-moves '(0 0 0) input))))

(define (find-max pos steps)
  (let loop ((max-pos '()) (max-steps 0) (current-pos pos) (head steps))
    (if (null? head) max-steps
      (let* ((next-pos (apply-move current-pos (car head)))
             (next-steps (steps-to-origin next-pos)))
        (if (> next-steps max-steps)
          (loop next-pos next-steps next-pos (cdr head))
          (loop max-pos max-steps next-pos (cdr head)))))))

(define (problem-2 path)
  (let ((input (string-split (car (string-split (read-all path) "\n")) ",")))
    (find-max '(0 0 0) input)))

(print (problem-1 "data/day11-1.txt"))
(print (problem-2 "data/day11-1.txt"))
