(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (elves '()) (count 0))
        (match line
          ((? eof-object?) (pk (apply max elves)))
          ((? string-null?) (lp (read-line port) (cons count elves) 0))
          (_ (lp (read-line port) elves (+ count (string->number line)))))))))

(walk "input1b.txt")
