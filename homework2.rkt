#lang scheme

;; Define the build-path function
(define (build-path rows)
  (cond
    ((null? rows) '())
    (else (cons (car rows) (build-path (cdr rows))))))

;; Sample path
(define sample-path
  (build-path
   '(("S" "-" "-" "-" "-")
     ("E" "-" "-" "-" "-")
     ("E" "E" "E" "E" "E")
     ("-" "-" "-" "-" "E")
     ("-" "-" "-" "-" "F"))))

;; Task 1: Define getHeight and getWidth
(define (getHeight path)
  (length path))

(define (getWidth path)
  (if (null? path)
      0
      (length (car path))))

;; Task 2: Define getLetter
(define (getLetter path row col)
  (if (or (< row 0) (>= row (getHeight path)) (< col 0) (>= col (getWidth path)))
      '()
      (list-ref (list-ref path row) col)))

;; Task 3: Define solvePath
(define (solvePath path)
  ;; Find the finish cell labeled "F"
  (define (findFinish path)
    (define (find-row rows r)
      (if (null? rows)
          '()
          (let ((c (find-col (car rows) 0)))
            (if c
                (cons r c)
                (find-row (cdr rows) (+ r 1))))))
    (define (find-col row c)
      (if (null? row)
          #f
          (if (equal? (car row) "F")
              (list c)
              (find-col (cdr row) (+ c 1)))))
    (find-row path 0))

  ;; Helper function to recursively find the path
  (define (helper pos finish visited)
    (let ((row (car pos))
          (col (cadr pos))
          (fr (car finish))
          (fc (cadr finish)))
      (if (or (member pos visited)
              (< row 0) (>= row (getHeight path))
              (< col 0) (>= col (getWidth path)))
          '()
          (let ((current-letter (getLetter path row col)))
            (if (equal? pos finish)
                '()
                (let ((new-visited (cons pos visited)))
                  (cond
                    ((equal? (getLetter path (+ row 1) col) "E")
                     (cons 'D (helper (list (+ row 1) col) finish new-visited)))
                    ((equal? (getLetter path row (+ col 1)) "E")
                     (cons 'R (helper (list row (+ col 1)) finish new-visited)))
                    ((equal? (getLetter path row (- col 1)) "E")
                     (cons 'L (helper (list row (- col 1)) finish new-visited)))
                    ((equal? (getLetter path (- row 1) col) "E")
                     (cons 'U (helper (list (- row 1) col) finish new-visited)))
                    (else '()))))))))

  (helper '(0 0) (findFinish path) '()))

;; Example usage
(display (solvePath sample-path)) ; Output should be (D D R R R R D D)
