(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (get-overlapping one two)         ; (2 8) (3 7)
  (or (<= (car two) (car one) (cadr two))
      (<= (car two) (cadr one) (cadr two))
      (<= (car one) (car two) (cadr two) (cadr one))
      (<= (car two) (car one) (cadr one) (cadr two))
      ))

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
                 (overlapping? (get-overlapping one two)))
            ;; (pk line ranges one two overlapping?)
            (lp (read-line port)
                (if overlapping? (1+ count) count)))))))))

(walk "input4b.txt")
