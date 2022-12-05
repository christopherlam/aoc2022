(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define crates-init
  (vector (list 'c 'q 'b)
          (list 'z 'w 'q 'r)
          (list 'v 'l 'r 'm 'b)
          (list 'w 't 'v 'h 'z 'c)
          (list 'g 'v 'n 'b 'h 'z 'd)
          (list 'q 'v 'f 'j 'c 'p 'n 'h)
          (list 's 'z 'w 'r 't 'g 'd)
          (list 'p 'z 'w 'b 'n 'm 'g 'c)
          (list 'p 'f 'q 'w 'm 'b 'j 'n)))

(define to-num (compose inexact->exact string->number))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (crates crates-init))
        (cond
         ((eof-object? line)
          (pk crates))
         (else
          (match (string-split line #\space)
            (("move" (= to-num num) "from" (= to-num from) "to" (= to-num to))
             (let ((old-from (vector-ref crates (1- from))))
               (when (null? old-from)
                 (error "invalid state, old-from empty"))
               (vector-set! crates (1- to) (append (list-head old-from num) (vector-ref crates (1- to))))
               (vector-set! crates (1- from) (list-tail old-from num))))
            (_ #f))
          (lp (read-line port) crates)))))))

(walk "input5b.txt")
