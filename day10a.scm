(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((cycle 1) (countdown 0) (x 1) (next-x 1) (tally 0))
        (cond
         ((= 0 countdown)
          (match (read-line port)
            ((? eof-object?) (pk tally))
            ((? string-null?) #f)
            (line (match (string-split line #\space)
                    (("noop")
                     (lp cycle 1 next-x next-x tally))
                    (("addx" (= string->number num))
                     (lp cycle 2 next-x (+ next-x num) tally))))))
         (else
          ;; (pk 'cycle cycle 'x x)
          (lp (1+ cycle) (1- countdown) x next-x
              (if (memv cycle '(20 60 100 140 180 220))
                  (+ tally (* cycle x))
                  tally))))))))

(walk "input10c.txt")
