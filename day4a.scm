(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (get-contained one two)         ; (4 5) (4 58)
  (or
   ;; scenario 1: one smaller than two
   (and (>= (car one) (car two))
        (<= (cadr one) (cadr two)))
   ;; scenario 2: one larger than two)
   (and (>= (car two) (car one))
        (<= (cadr two) (cadr one)))))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (count 0))
        (cond
         ((or (eof-object? line) (string-null? line))
          (pk count))
         (else
          (let* ((ranges (string-split line #\,))
                 (one (map string->number (string-split (car ranges) #\-)))
                 (two (map string->number (string-split (cadr ranges) #\-)))
                 (contained? (get-contained one two)))
            ;; (pk line ranges one two contained?)
            (lp (read-line port)
                (if contained? (1+ count) count)))))))))

(walk "input4b.txt")
