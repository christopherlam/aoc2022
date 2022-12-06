(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define all-different
  (match-lambda
    (() #t)
    ((head . tail) (and (not (member head tail)) (all-different tail)))))

(define (collect-from port num accum)
  (if (zero? num)
      accum
      (collect-from port (1- num) (cons (read-char port) accum))))

(define (walk file num)
  (call-with-input-file file
    (lambda (port)
      (let lp ((trailing-list (collect-from port num '())) (pos num))
        (if (all-different trailing-list)
            pos
            (lp (take (cons (read-char port) trailing-list) num) (1+ pos)))))))

(pk (walk "input6b.txt" 14))
