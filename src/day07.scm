(require-extension srfi-1)
(require-extension utils)

; (define test '(a ((b ()) (c ()) (d ((e ()))))))

(define (tree-insert root parent-name new-node)
  (if (equal? (car root) parent-name)
    (list (car root) (cons new-node (cadr root)))
    (list (car root) (map (lambda (node) (tree-insert node parent-name new-node)) (cadr root)))))

(define (tree-find root name)
  (if (equal? (car root) name) root
    (find (lambda (node) (tree-find node name)) (cadr root))))

(define (tree-filter nodes name)
  (if (null? nodes) nodes
    (map (lambda (node) (list (car node) (tree-filter (cadr node) name)))
         (filter (lambda (node) (not (equal? (car node) name))) nodes))))

(define (tree-pluck root names)
  (let ((existing (filter (lambda (e) (not (equal? e #f))) (map (lambda (name) (tree-find root name)) names))))
    (let ((new-root (fold (lambda (node a-root) (list (car a-root) (tree-filter (cadr a-root) (car node)))) root existing)))
      (list new-root existing))))

(define (build-flat-tree tbl)
  (let loop ((remaining tbl) (root (list 'ROOT '())))
    (if (null? remaining) root
      (loop (cdr remaining) (tree-insert root (car root) (list (caar remaining) '()))))))

(define (build-structure-tree flat-tree tbl)
  (let loop ((remaining (cdr tbl)) (root flat-tree))
    (if (null? remaining) root
      (let* ((pluck-result (tree-pluck root (cddr (car remaining))))
             (new-root (car pluck-result))
             (nodes (cadr pluck-result)))
        (loop (cdr remaining) (fold (lambda (node a-root) (tree-insert a-root (caar remaining) node)) new-root nodes))))))

(define (parse l) (string-split l " ,->()"))

(define (problem-1 path)
  (let ((input (map parse (string-split (read-all path) "\n"))))
    (build-structure-tree (build-flat-tree input) input)))

(print (caaadr (problem-1 "data/day07-1.txt")))

(define (balanced? root lookup)
  (let ((weight (string->number (cadr (assoc (car root) lookup)))))
    (if (null? (cadr root)) weight
      (let ((child-weights (map (lambda (node) (balanced? node lookup)) (cadr root))))
        (if (null? (filter (lambda (w) (not (= w (car child-weights)))) child-weights))
          (+ weight (fold + 0 child-weights))
          (let ((diff (zip (map (lambda (node) (string->number (cadr (assoc (car node) lookup)))) (cadr root)) child-weights)))
            (error diff)))))))

(define (problem-2 path)
  (let ((input (map parse (string-split (read-all path) "\n"))))
    (let ((root (caadr (build-structure-tree (build-flat-tree input) input))))
      (balanced? root input))))

(problem-2 "data/day07-1.txt")
