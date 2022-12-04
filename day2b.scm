(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

(define letter-symbol-alist
  '((#\A . "rock")
    (#\B . "paper")
    (#\C . "scissors")
    (#\X . "rock")
    (#\Y . "paper")
    (#\Z . "scissors")))

(define shape-score-alist
  '(("rock" . 1) ("paper" . 2) ("scissors" . 3)))

(define (determine-from-L1 L1 R)
  (match L1
    ("rock" (match R (#\X "scissors") (#\Y "rock") (#\Z "paper")))
    ("paper" (match R (#\X "rock") (#\Y "paper") (#\Z "scissors")))
    ("scissors" (match R (#\X "paper") (#\Y "scissors") (#\Z "rock")))))

(define (to-score L R)
  (let* ((L1 (assoc-ref letter-symbol-alist L))
         (R1 (determine-from-L1 L1 R))
         (score-shape (assoc-ref shape-score-alist R1))
         (score-round
          (if (equal? L1 R1)
              3
              (match L1
                ("rock"     (match R1 ("paper" 6) ("scissors" 0)))
                ("paper"    (match R1 ("rock" 0)  ("scissors" 6)))
                ("scissors" (match R1 ("paper" 0) ("rock" 6)))))))
    ;; (pk L R L1 R1 shape-score-alist score-shape score-round)
    (+ score-shape score-round)))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (score 0))
        (match line
          ((? eof-object?) (pk score))
          (_ (lp (read-line port)
                 (+ score (to-score (string-ref line 0) (string-ref line 2))))))))))

(walk "input2b.txt")
