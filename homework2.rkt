#lang scheme

; Helper function to build the path (provided in the homework)
(define (buildPath rows)
  (cond
    ((null? rows) null)
    (else (cons (car rows) (buildPath (cdr rows))))))

; Sample path given in the homework
(define sample-path (buildPath
  '(("S" "-" "-" "-" "-")
    ("E" "-" "-" "-" "-")
    ("E" "E" "E" "E" "E")
    ("-" "-" "-" "-" "E")
    ("-" "-" "-" "-" "F"))))

; Task 1: Define getHeight function
(define (getHeight path)
  (length path))

; Task 1: Define getWidth function
(define (getWidth path)
  (if (null? path)
      0
      (length (car path))))

; Task 2: Define getLetter function
(define (getLetter path row col)
  (list-ref (list-ref path row) col))

; Task 3: Define solvePath function
(define (solvePath path)
  (define directions '((0 1 R) (1 0 D) (0 -1 L) (-1 0 U)))
  (define height (getHeight path))
  (define width (getWidth path))
  
  (define (in-bounds? row col)
    (and (>= row 0) (< row height) (>= col 0) (< col width)))
  
  (define (find-path row col visited)
    (if (equal? (getLetter path row col) "F")
        '()
        (let loop ((dirs directions))
          (if (null? dirs)
              #f
              (let* ((dir (car dirs))
                     (new-row (+ row (car dir)))
                     (new-col (+ col (cadr dir)))
                     (move (caddr dir)))
                (if (and (in-bounds? new-row new-col)
                         (not (member (list new-row new-col) visited))
                         (or (equal? (getLetter path new-row new-col) "E")
                             (equal? (getLetter path new-row new-col) "F")))
                    (let ((result (find-path new-row new-col (cons (list new-row new-col) visited))))
                      (if result
                          (cons move result)
                          (loop (cdr dirs))))
                    (loop (cdr dirs))))))))
  (find-path 0 0 '((0 0))))

; Testing the functions
(display (getHeight sample-path)) ; should return 5
(newline)
(display (getWidth sample-path)) ; should return 5
(newline)
(display (getLetter sample-path 0 0)) ; should return "S"
(newline)
(display (getLetter sample-path 1 0)) ; should return "E"
(newline)
(display (solvePath sample-path)) ; should return (D D R R R R D D)
(newline)
