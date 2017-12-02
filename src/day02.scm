(require-extension utils)
(require-extension srfi-1)

(define (parse-input path)
  (call-with-input-file path
    (lambda (p)
      (let ((newline-split (string-split (read-all p) "\n")))
        (map (lambda (s) (map string->number (string-split s "\t"))) newline-split)))))

(define (problem-1 t)
  (let ((max-min-pairs (map (lambda (l) (list (apply max l) (apply min l))) t)))
    (fold + 0 (map (lambda (p) (- (car p) (cadr p))) max-min-pairs))))

(define (product-set l1 l2)
  (foldr append '() (map (lambda (i1) (map (lambda (i2) (list i1 i2)) l2)) l1)))

(define (find-div l)
  (let ((div-pairs (filter (lambda (p) (not (= (car p) (cadr p)))) (product-set l l))))
    (find (lambda (p) (integer? (/ (car p) (cadr p)))) div-pairs)))

(define (problem-2 t)
  (fold + 0 (map (lambda (p) (/ (car p) (cadr p))) (map find-div t))))

(print (problem-1 (parse-input "data/day02-1.txt")))
(print (problem-2 (parse-input "data/day02-2.txt")))
