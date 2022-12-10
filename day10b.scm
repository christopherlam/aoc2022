(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (newline? cycle)
  (memv cycle '(40 80 120 160 200 240)))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((cycle 1) (countdown 0) (x 1) (next-x 1) (col 0))
        (cond
         ((= 0 countdown)
          (match (read-line port)
            ((? eof-object?) #f)
            ((? string-null?) #f)
            (line (match (string-split line #\space)
                    (("noop")
                     (lp cycle 1 next-x next-x col))
                    (("addx" (= string->number num))
                     (lp cycle 2 next-x (+ next-x num) col))))))
         (else
          (display (if (< (abs (- col x)) 2) "#" "."))
          (if (newline? cycle)
              (newline))
          (lp (1+ cycle) (1- countdown) x next-x
              (if (newline? cycle) 0 (1+ col)))))))))

(walk "input10c.txt")
