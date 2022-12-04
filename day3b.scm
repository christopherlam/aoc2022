(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (find-badge lines-accum)
  (define hashes-map
    (map
     (lambda (s)
       (define h (make-hash-table))
       (string-for-each (lambda (c) (hash-set! h c #t)) s)
       h)
     lines-accum))
  ;; (define (hash-pk hash) (pk (hash-fold (lambda (k v p) (cons k p)) '() hash)))
  ;; (pk lines-accum)
  ;; (for-each hash-pk hashes-map)
  (hash-fold
   (lambda (k v p) (cons k p))
   '()
   (fold
    (lambda (a b)
      (let ((h (make-hash-table)))
        (hash-for-each
         (lambda (k v) (if (hash-ref b k) (hash-set! h k #t)))
         a)
        h))
    (car hashes-map) (cdr hashes-map))))

(define (common->priority c)
  (let ((i (char->integer c)))
    (cond
     ((<= 97 i 122) (- i 96))
     ((<= 65 i 90)  (- i 38))
     (else #f))))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (lines-accum '()) (score 0))
        (cond
         ((= (length lines-accum) 3)
          (lp (read-line port) (list line) (+ (common->priority (car (find-badge lines-accum))) score)))
         ((eof-object? line)
          (pk score))
         (else
          (lp (read-line port) (cons line lines-accum) score)))))))

(walk "input3b.txt")
