(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (elves '()) (count 0))
        (match line
          ((? eof-object?) (pk (apply + (take (sort elves >) 3))))
          ((? string-null?) (lp (read-line port) (cons count elves) 0))
          (_ (lp (read-line port) elves (+ count (string->number line)))))))))

(walk "input1b.txt")
