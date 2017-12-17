(require-extension utils)
(require-extension srfi-1)

(define (compare-bits n1 n2 c)
  (let loop ((b 0))
    (let ((v (arithmetic-shift 1 b)))
      (if (>= b c) #t
        (if (= (bitwise-and n1 v) (bitwise-and n2 v))
          (loop (+ b 1)) #f)))))

(define (count-match a b af bf n match)
  (if (= n 0) match
    (let ((next-a (remainder (* a af) 2147483647))
          (next-b (remainder (* b bf) 2147483647)))
      (count-match next-a next-b af bf (- n 1) (if (compare-bits a b 16) (+ match 1) match)))))

(define (count-match-2 a b af bf n match)
  (if (= n 0) match
    (let ((next-a (remainder (* a af) 2147483647))
          (next-b (remainder (* b bf) 2147483647)))
      (cond
        ((and (= (remainder a 4) 0) (= (remainder b 8) 0))
         (count-match-2 next-a next-b af bf (- n 1) (if (compare-bits a b 16) (+ match 1) match)))
        ((= (remainder a 4) 0)
         (count-match-2 a next-b af bf n match))
        ((= (remainder b 8) 0)
         (count-match-2 next-a b af bf n match))
        (else
          (count-match-2 next-a next-b af bf n match))))))

(print (count-match 783 325 16807 48271 40000000 0))
(print (count-match-2 783 325 16807 48271 5000000 0))
