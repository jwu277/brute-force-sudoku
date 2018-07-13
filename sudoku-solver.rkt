;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require racket/list) ;gets list-ref, take and drop

;;
;; Brute force Sudoku solver
;;
;; In Sudoku, the board is a 9x9 grid of SQUARES.
;; There are 9 ROWS and 9 COLUMNS, there are also 9
;; 3x3 BOXES.  Rows, columns and boxes are all UNITs.
;; So there are 27 units.
;;
;; The idea of the game is to fill each square with
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.
;;

;; =================
;; Data definitions:


;; Val is Natural[1, 9]

;; Board is (listof Val|false)   that is 81 elements long
;; interp.
;;  Visually a board is a 9x9 array of squares, where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; Pos is Natural[0, 80]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 9)
;;    - the column is (remainder p 9)


;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 9) c))  ;helpful for writing tests


;; Unit is (listof Pos) of length 9
;; interp. 
;;  The position of every square in a unit. There are
;;  27 of these for the 9 rows, 9 columns and 9 boxes.


;; =================
;; Constants:

(define ALL-VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ;B stands for blank


(define BD1 
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD2 
  (list 1 2 3 4 5 6 7 8 9 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD3 
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define BD4                ;easy
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))

(define BD4s               ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))

(define BD5                ;hard
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B 
        B B B B B 6 B B B 
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))

(define BD5s               ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))

(define BD6                ;hardest ever? (Dr Arto Inkala)
  (list B B 5 3 B B B B B 
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B 
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))

(define BD7                 ; no solution 
  (list 1 2 3 4 5 6 7 8 B 
        B B B B B B B B 2 
        B B B B B B B B 3 
        B B B B B B B B 4 
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))




;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))




;; =================
;; Functions:

;; Board -> Board or false
;; produces a solution (filled board) for bd or false if bd is unsolveable
;; Assume: bd is (starts) valid

(check-expect (solve BD4) BD4s)
(check-expect (solve BD5) BD5s)
(check-expect (solve BD7) false)

(define (solve bd)
  
  (local
    [(define (solve--bd bd)
       (if (solved? bd)
           bd
           (solve--lobd (next-boards bd))))
     
     (define (solve--lobd lobd)
       (cond [(empty? lobd) false]
             [else
              (local [(define try (solve--bd (first lobd)))]
                (if (not (false? try))
                    try
                    (solve--lobd (rest lobd))))]))]
    
    (solve--bd bd)))


;; Board -> Boolean
;; produces true if consumed board is full. false otherwise

(check-expect (solved? BD1) false)
(check-expect (solved? BD4) false)
(check-expect (solved? BD4s) true)
(check-expect (solved? BD5s) true)
(check-expect (solved? BD7) false)

(define (solved? bd) (not (ormap false? bd)))


;; Board (non-full) -> (listof Board)
;; produces list of valid next boards
;; by filling lowest empty cell with Natural[0,9] and retaining valid boards
;; ASSUME: bd is not full

(check-expect (next-boards BD1) (list (list 1 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 2 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 3 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 4 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 5 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 6 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 7 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 8 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)
                                      (list 9 B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B
                                            B B B B B B B B B)))
(check-expect (next-boards BD4) (list (list 2 7 4 6 9 1 B B 5
                                            1 B B 5 B B B 9 B
                                            6 B B B B 3 2 8 B
                                            B B 1 9 B B B B 8
                                            B B 5 1 B B 6 B B
                                            7 B B B 8 B B B 3
                                            4 B 2 B B B B B 9
                                            B B B B B B B 7 B
                                            8 B B 3 4 9 B B B)
                                      (list 2 7 4 8 9 1 B B 5
                                            1 B B 5 B B B 9 B
                                            6 B B B B 3 2 8 B
                                            B B 1 9 B B B B 8
                                            B B 5 1 B B 6 B B
                                            7 B B B 8 B B B 3
                                            4 B 2 B B B B B 9
                                            B B B B B B B 7 B
                                            8 B B 3 4 9 B B B)))
(check-expect (next-boards BD7) empty)

(define (next-boards bd) (filter valid? (local
                                          [(define (add-to-board n)
                                             (fill-next bd 0 n))]
                                          (map add-to-board (build-list 9 add1)))))


;; Board -> Boolean
;; produces false if bd contradicts any unit. true otherwise

(check-expect (valid? BD1) true)
(check-expect (valid? BD4) true)
(check-expect (valid? BD5s) true)
(check-expect (valid? BD7) true)
(check-expect (valid? (list 2 7 4 9 9 1 B B 5
                            1 B B 5 B B B 9 B
                            6 B B B B 3 2 8 B
                            B B 1 9 B B B B 8
                            B B 5 1 B B 6 B B
                            7 B B B 8 B B B 3
                            4 B 2 B B B B B 9
                            B B B B B B B 7 B
                            8 B B 3 4 9 B B B)) false)
(check-expect (valid? (list 1 2 3 4 5 6 7 8 4 
                            B B B B B B B B 2 
                            B B B B B B B B 3 
                            B B B B B B B B 4 
                            B B B B B B B B 5
                            B B B B B B B B 6
                            B B B B B B B B 7
                            B B B B B B B B 8
                            B B B B B B B B 9)) false)

(define (valid? bd) (andmap (lambda [u] (satisfies-unit? bd u)) UNITS))


;; Board Unit -> Boolean
;; produces true if bd has no duplicates within given Unit, u. false otherwise

(check-expect (satisfies-unit? BD1 (first ROWS)) true)
(check-expect (satisfies-unit? BD1 (first BOXES)) true)
(check-expect (satisfies-unit? BD4 (first COLS)) true)
(check-expect (satisfies-unit? BD5s (first BOXES)) true)
(check-expect (satisfies-unit? (list 1 2 3 4 5 6 7 8 4 
                                     B B B B B B B B 2 
                                     B B B B B B B B 3 
                                     B B B B B B B B 4 
                                     B B B B B B B B 5
                                     B B B B B B B B 6
                                     B B B B B B B B 7
                                     B B B B B B B B 8
                                     B B B B B B B B 9) (first ROWS)) false)
(check-expect (satisfies-unit? (list 1 2 3 4 5 6 7 8 4 
                                     B B B B B B B B 2 
                                     B B B B B B B B 3 
                                     B B B B B B B B 4 
                                     B B B B B B B B 5
                                     B B B B B B B B 6
                                     B B B B B B B B 7
                                     B B B B B B B B 8
                                     B B B B B B B B 9) (list 8 17 26 35 44 53 62 71 80)) false)
(check-expect (satisfies-unit? (list 1 2 3 4 5 6 7 8 4 
                                     B B B B B B B B 2 
                                     B B B B B B B B 3 
                                     B B B B B B B B 4 
                                     B B B B B B B B 5
                                     B B B B B B B B 6
                                     B B B B B B B B 7
                                     B B B B B B B B 8
                                     B B B B B B B B 9) (first BOXES)) true)

(define (satisfies-unit? bd u) (not (duplicates? (cell-list bd u))))


;; (listof Natural) -> Boolean
;; produces true if a natural appears at least twice in lon. false otherwise

(check-expect (duplicates? empty) false)
(check-expect (duplicates? (list 31 2 3)) false)
(check-expect (duplicates? (list 10 4 7 4)) true)
(check-expect (duplicates? (list 3 1 1 29 1 0)) true)
(check-expect (duplicates? (list 4 15 4 9 8 9)) true)

(define (duplicates? lon)
  (cond [(empty? lon) false]
        [else
         (or (in-list? (first lon) (rest lon))
             (duplicates? (rest lon)))]))


;; Natural (listof Natural) -> Boolean
;; produces true if n is in lon. false otherwise

(check-expect (in-list? 20 empty) false)
(check-expect (in-list? 10 (list 3 2 1)) false)
(check-expect (in-list? 13 (list 4 13 1 1 8)) true)
(check-expect (in-list? 7 (list 7 7 6 7)) true)

(define (in-list? n lon) (ormap (lambda [k] (= n k)) lon))


;; Board (listof Natural[0,80]) -> (listof Natural[1,9])
;; produces the list of natural numbers that occupy the cells described by u in bd
;; ASSUME: no natural appears twice or more in u

(check-expect (cell-list BD1 (first ROWS)) empty)
(check-expect (cell-list BD1 (list 13 2 3 10 18)) empty)
(check-expect (cell-list BD5 (first ROWS)) (list 5 4 7))
(check-expect (cell-list BD5 (list 4 5 6 7 8)) (list 4 7))
(check-expect (cell-list BD5s (first BOXES)) (list 5 3 9 8 1 2 6 7 4))
(check-expect (cell-list BD7 (list 54 55 56 63 64 65 72 73 74)) empty)

(define (cell-list bd u)
  (cond [(empty? u) empty]
        [else (local
                [(define try (read-square bd (first u)))]
                (if (false? try)
                    (cell-list bd (rest u))
                    (cons try (cell-list bd (rest u)))))]))


;; Board (non-full) Natural[1,9] -> Board
;; fills the smallest (in index) empty cell with n
;; ASSUME: bd is not full

(check-expect (fill-next BD1 0 3) (list 3 B B B B B B B B
                                        B B B B B B B B B
                                        B B B B B B B B B
                                        B B B B B B B B B
                                        B B B B B B B B B
                                        B B B B B B B B B
                                        B B B B B B B B B
                                        B B B B B B B B B
                                        B B B B B B B B B))
(check-expect (fill-next BD4 0 9) (list 2 7 4 9 9 1 B B 5
                                        1 B B 5 B B B 9 B
                                        6 B B B B 3 2 8 B
                                        B B 1 9 B B B B 8
                                        B B 5 1 B B 6 B B
                                        7 B B B 8 B B B 3
                                        4 B 2 B B B B B 9
                                        B B B B B B B 7 B
                                        8 B B 3 4 9 B B B))
(check-expect (fill-next BD4 23 7) (list 2 7 4 B 9 1 B B 5
                                         1 B B 5 B B B 9 B
                                         6 B B B B 3 2 8 7
                                         B B 1 9 B B B B 8
                                         B B 5 1 B B 6 B B
                                         7 B B B 8 B B B 3
                                         4 B 2 B B B B B 9
                                         B B B B B B B 7 B
                                         8 B B 3 4 9 B B B))

(define (fill-next bd pos n)
  (if (false? (read-square bd pos))
      (fill-square bd pos n)
      (fill-next bd (add1 pos) n)))


;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square BD2 (r-c->pos 0 5)) 6)
(check-expect (read-square BD3 (r-c->pos 7 0)) 8)

(define (read-square bd p)
  (list-ref bd p))


;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square BD1 (r-c->pos 0 0) 1)
              (cons 1 (rest BD1)))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))