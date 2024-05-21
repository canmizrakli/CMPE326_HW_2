#lang scheme

(define (getHeight path)
  (length path))

(define (getWidth path)
  (if (null? path)
      0
      (length (car path))))

(define (getLetter path row col)
  (list-ref (list-ref path row) col))

(define (find-start path)
  (let loop ((row 0) (rows path))
    (if (null? rows)
        #f
        (let ((col (list-index (lambda (x) (equal? x "S")) (car rows))))
          (if col
              (list row col)
              (loop (+ row 1) (cdr rows)))))))

(define (list-index pred lst)
  (let loop ((lst lst) (i 0))
    (cond
     ((null? lst) #f)
     ((pred (car lst)) i)
     (else (loop (cdr lst) (+ i 1))))))

(define (in-boundaries? row col height width)
  (and (>= row 0) (< row height) (>= col 0) (< col width)))

(define (move pos dir)
  (case dir
    ((U) (list (- (car pos) 1) (cadr pos)))
    ((D) (list (+ (car pos) 1) (cadr pos)))
    ((L) (list (car pos) (- (cadr pos) 1)))
    ((R) (list (car pos) (+ (cadr pos) 1)))))

(define (solve current path height width visited)
  (let loop ((pos current) (visited visited) (moves '()))
    (let ((row (car pos)) (col (cadr pos)))
      (if (equal? (getLetter path row col) "F")
          (reverse moves)
          (let ((directions '(U D L R)))
            (foldl (lambda (dir acc)
                     (let ((new-pos (move pos dir)))
                       (if (and (not (memv new-pos visited))
                                (in-boundaries? (car new-pos) (cadr new-pos) height width)
                                (not (equal? (getLetter path (car new-pos) (cadr new-pos)) "-")))
                           (let ((result (loop new-pos (cons new-pos visited) (cons dir moves))))
                             (if result result acc))
                           acc)))
                   #f
                   directions))))))

(define (solvePath path)
  (let* ((height (getHeight path))
         (width (getWidth path))
         (start (find-start path)))
    (if start
        (solve start path height width '())
        (error "Start position not found"))))

(define (buildPath rows)
  (cond
    ((null? rows) '())
    (else (cons (car rows) (buildPath (cdr rows))))))

(define sample-path
  (buildPath
   '(("S" "-" "-" "-" "-")
     ("E" "-" "-" "-" "-")
     ("E" "E" "E" "E" "E")
     ("-" "-" "-" "-" "E")
     ("-" "-" "-" "-" "F"))))

(display (solvePath sample-path)) ; should display (D D R R R R D D)
(newline)
