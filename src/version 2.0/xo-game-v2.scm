;; XO game

;; version 2.0
;;  separate code for easy control program, following 
;;  - xo_game.scm use to control game
;;  - xo_basic.scm contains AI algorithm version 1.0

;; AI algorithms
(require "xo-beginner.ss")

;; Simulation game play
;; example:

;; > (play '(- - - X O - - - -))
;; Player O play -> (- - - X O - - O -)
;;  - - -
;;  X O -
;;  - O -
;; play next round ..

;; > (play '(- O X X O - - - -))
;; Player O play -> (- O X X O - - O -)
;;  - O X
;;  X O -
;;  - O -
;; !!!!! Player O is the WINNER !!!!!

;; Game play
(define (play state)
  (let* ((player (whos-next state))
         (path (find-next-way state)))
    
    ;; body
    (display "Player ")
    (display player)
    (display " play -> ")
    (display path)
    (newline)
    (display-table path)
    ;; check player is the winner ?
    (cond ((win? path player)
           (begin
             (display "!!!!! Player ")
             (display player)
             (display " is the WINNER !!!!!")))
          ((and (number? path) (= path 0)) 
           (begin
             (display "don't play")
             (newline)
             (display-table state)))
          (else 
           (display "play next round ..")))
    (newline)))

;; table simulation
(define (display-table path)
  (cond ((null? path) (display ""))
        ((and (number? path)(= path 0)) (display ""))
        ((= (modulo (length path) 3) 1)
         (begin 
           (display " ")
           (display (car path))
           (newline)
           (display-table (cdr path))))
        (else
         (begin
           (display " ")
           (display (car path))
           (display-table (cdr path))))))