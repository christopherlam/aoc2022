
(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

(define (grid-get grid row col)
  (- (char->integer (vector-ref (vector-ref grid col) row)) 48))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (grid '()))
        (cond
         ((eof-object? line) (list->vector (reverse grid)))
         (else (lp (read-line port)
                   (cons (list->vector (string->list line)) grid))))))))

(define (visible? newval oldval)
  (and oldval (< newval oldval) oldval))

(define (is-visible-from? grid row col direction) ;direction: 'n 's 'e 'w
  (define maxrow (1- (vector-length grid)))
  (define maxcol (1- (vector-length (vector-ref grid 0))))
  (let lprow ((row row) (col col) (rowval (grid-get grid row col)))
    (and rowval
         (case direction
           ((n) (if (= 0 row)
                    rowval
                    (lprow (1- row) col
                           (visible? (grid-get grid (1- row) col) rowval))))
           ((s) (if (= row maxrow)
                    rowval
                    (lprow (1+ row) col
                           (visible? (grid-get grid (1+ row) col) rowval))))
           ((w) (if (= col 0)
                    rowval
                    (lprow row (1- col)
                           (visible? (grid-get grid row (1- col)) rowval))))
           ((e) (if (= col maxcol)
                    rowval
                    (lprow row (1+ col)
                           (visible? (grid-get grid row (1+ col)) rowval))))))))

(define (is-visible? grid row col)
  ;; (pk 'is-visible? 'grid row col)
  (if (or (is-visible-from? grid row col 'n)
          (is-visible-from? grid row col 's)
          (is-visible-from? grid row col 'e)
          (is-visible-from? grid row col 'w))
      1 0))

(define (find-visible grid)
  (define maxrow (1- (vector-length grid)))
  (define maxcol (1- (vector-length (vector-ref grid 0))))
  (let lp ((row 0) (col 0) (numvisible (is-visible? grid 0 0)))
    ;; (pk 'maxrow maxrow 'maxcol maxcol 'r row 'c col 'n numvisible)
    (cond
     ((= col maxcol)
      (if (= row maxrow)
          numvisible
          (lp (1+ row) 0 (+ numvisible (is-visible? grid (1+ row) 0)))))
     (else (lp row (1+ col) (+ numvisible (is-visible? grid row (1+ col))))))))

(define grid (walk "input8b.txt"))

(pk 'visible (find-visible grid))
