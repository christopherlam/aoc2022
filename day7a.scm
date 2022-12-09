(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (make-path current subdir)
  (reverse (cons subdir current)))

(define (has-prefix? path prefix)
  (match (list path prefix)
    ((() _) #f)
    ((_ ()) #t)
    (((a . rpath) (a . rprefix)) (has-prefix? rpath rprefix))
    (_ #f)))

(define (path-size files prefix)
  (let lp ((files files) (size 0))
    (cond
     ((null? files) size)
     ((and (has-prefix? (caar files) prefix) (number? (cadar files)))
      (lp (cdr files) (+ size (cadar files))))
     (else (lp (cdr files) size)))))

(define (walk file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((line (read-line port)) (current '()) (files '()))
        (cond
         ((eof-object? line)
          (let lp1 ((files-lp files) (size 0))
            (match files-lp
              (() (pk 'size size))
              (((path 'dir) . rest)
               (let ((dirsize (path-size files path)))
                 (if (<= dirsize 100000)
                     (lp1 rest (+ size dirsize))
                     (lp1 rest size))))
              ((_ . rest) (lp1 rest size)))))
         (else
          (match (string-split line #\space)
            (("$" "cd" "..") (lp (read-line port) (cdr current) files))
            (("$" "cd" path) (lp (read-line port) (cons path current) files))
            (("$" "ls")      (let lp1 ((line (read-line port)) (files files))
                               (if (eof-object? line)
                                   (lp line current files)
                                   (match (string-split line #\space)
                                     (("dir" subdir) (lp1 (read-line port) (cons (list (make-path current subdir) 'dir) files)))
                                     ((size filename) (lp1 (read-line port) (cons (list (make-path current filename) (string->number size)) files)))
                                     (_ (lp line current files))))))
            (line (error line)))))))))

(walk "input7b.txt")
