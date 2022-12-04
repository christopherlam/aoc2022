(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

(define (find-common line)
  (let* ((len/2 (/ (string-length line) 2))
         (part1 (string-take line len/2))
         (part2 (string-take-right line len/2))
         (h (make-hash-table))
         (rv #f))
    (string-for-each (lambda (c) (hash-set! h c #t)) part1)
    (string-for-each (lambda (c) (if (hash-ref h c) (set! rv c))) part2)
    rv))

(define (common->priority c)
  (let ((i (char->integer c)))
    (cond
     ((<= 97 i 122) (- i 96))
     ((<= 65 i 90)  (- i 38))
     (else #f))))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (score 0))
        (match line
          ((? eof-object?) (pk score))
          (_ (let* ((common (find-common line))
                    (priority (common->priority common)))
               ;; (pk line common priority)
               (lp (read-line port)
                   (+ priority score)))))))))

(walk "input3b.txt")
