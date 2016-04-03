;; XO game

;; version 1.0
;;  find path to win
(require (lib "list.ss")
         (lib "compat.ss"))

;; define state
;; (- - - - - - - - -)  -->  empty board
;; (- - - - O - - - -)  -->  O put on index 4
;; (- - X - O - - - -)  -->  X put on index 2
;; (- - X O O - - - -)  -->  O put on index 3
;; (- - X O O X - - -)  -->  X put on index 5
;; (- - X O O X - - O)  -->  O put on index 8
;; (- X X O O X - - O)  -->  X put on index 1
;; (O X X O O X - - O)  -->  O put on index 0
;; (O X X O O X X - O)  -->  X put on index 6
;; (O X X O O X X O O)  -->  O put on index 7
;; result is drawn!

;; version 1.0
;; state space :: Get-MOVES
;; define avaible is '- to check the available space 
(define available '-)
(define P1 'O)
(define P2 'X)

;; function whos-next
;; find next player with begin by st player

;; simple decision put X or O
;; 9 - (number of -)
;; if odd then put 'who second  => 'X
;; otherwise   put 'who first   => 'O

;; input parameters
;;   state is current state
;;   st is first player
;;   nd is second player
(define (whos-next state)
  (if (= (modulo (num-space state) 2) 1)
      P1 P2))

(define (num-space state)
  (cond ((null? state) 0)
        ((eq? (car state) available)
         (+ 1 (num-space (cdr state)))) 
        (else 
         (num-space (cdr state)))))

;; ------------------- GET-MOVES ------------------------
(define (GET-MOVES state)
  (get-moves state (whos-next state)))

(define (get-moves state x)
  (define (moves-path index)
    (cond ((>= index (length state)) '())
          (else
           (if (eq? (list-ref state index) available)
               (cons (list (replace state index x) 1)
                     (moves-path (+ index 1)))
               (moves-path (+ index 1))
           ))))
  
  (moves-path 0)) 

;; replace <list> <index> <new atom>
(define (replace lst index x)
  (cond ((null? lst) '())
        ((= index 0)
         (cons x (replace (cdr lst) (- index 1) x)))
        (else
         (cons (car lst)
               (replace (cdr lst) (- index 1) x)))))

;; ------------------ End GET-MOVES ----------------------

;; define Goal States
;; (X X X - - - - - -)  ;; horizontal
;; (- - - X X X - - -)  ;; horizontal
;; (- - - - - - X X X)  ;; horizontal
;; (X - - X - - X - -)  ;; vertical
;; (- X - - X - - X -)  ;; vertical
;; (- - X - - X - - X)  ;; vertical
;; (X - - - X - - - X)  ;; cross
;; (- - X - X - X - -)  ;; cross
(define Goal '((X ((X X X - - - - - -)
                   (- - - X X X - - -)
                   (- - - - - - X X X)
                   (X - - X - - X - -)
                   (- X - - X - - X -)
                   (- - X - - X - - X)
                   (X - - - X - - - X)
                   (- - X - X - X - -)))
               (O ((O O O - - - - - -)
                   (- - - O O O - - -)
                   (- - - - - - O O O)
                   (O - - O - - O - -)
                   (- O - - O - - O -)
                   (- - O - - O - - O)
                   (O - - - O - - - O)
                   (- - O - O - O - -)))))

;; get all goal state of X/O
(define (Get-GoalStates x)
  (cadr (assoc x Goal)))

;; Heuristic
(define (Heuristic state goalstate)
    (match state goalstate 0))

;; count number of match
(define (match state1 goal count)
  (cond ((null? state1) count)
        ((eq? (car state1) available)
         (match (cdr state1) (cdr goal) count))
        ((not (eq? (car goal) available))
         (if (eq? (car state1) (car goal)) 
             (match (cdr state1) (cdr goal) (+ count 1))
             (match (cdr state1) (cdr goal) (- count 5))))   
        (else
         (match (cdr state1) (cdr goal) count))))

;; return #t/#f
(define (match? state goalstate)
  (= (match state goalstate 0) 3))


;; Searching by A*
;; Algorthm A Implementation
(define (a-tree startState goalState)

  (define (a-paths paths)
    (cond ((null? paths) #f)
          ((< (match (cadar paths) goalState 0) 0) #f)
          
          ((match? (cadar paths) goalState) (car paths))
          (else (a-paths (sort better-path
                               (append (cdr paths)
                                       (extend-all (car paths) 
                                                   (GET-MOVES (cadar paths)))))))))
  
  (define (extend-all path nextStates)
    (cond ((null? nextStates) '())
          ((member (caar nextStates) path) (extend-all path (cdr nextStates)))
          (else (cons (cons (+ (car path) (cadar nextStates))
                            (cons (caar nextStates) (cdr path)))
                      (extend-all path (cdr nextStates))))))
  
  (define (better-path path1 path2)
    (< (+ (car path1) (- (Heuristic (cadr path1) goalState)))
       (+ (car path2) (- (Heuristic (cadr path2) goalState)))))
  
  (a-paths (list (list 0 startState))))


;; -- find next path
(define (find-next-way current-state)
  (let*
      ;; local variables
      ((player (whos-next current-state))
       (goals (Get-GoalStates player))
       (all-paths (all-possible-paths current-state goals)))
    ;; body
    ;; sort operator
    (define (better lst1 lst2)
      (< (car lst1)
         (car lst2)))
    (if (not (null? all-paths))
        ;; return the best path ...
        (cadr (reverse (car (sort better all-paths)))))
    ))

(define (all-possible-paths state goals)
  (cond ((null? goals) '())
        (else
         (let ((path (a-tree state (car goals))))
           (if path
               (cons path (all-possible-paths state (cdr goals)))
               (all-possible-paths state (cdr goals)))
           ))))

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
           (display "..."))
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

;; win? 
;; this function used to check current player is the winner or not
(define (win? path player)
  (let ((goals (Get-GoalStates player)))
    ;; define private function
    (define (check-to-win all-goals)
      (cond ((null? all-goals) #f)
            ((and (number? path)(= path 0)) #f)
            ((match? path (car all-goals)) #t)
            (else
             (check-to-win (cdr all-goals)))))
    
    ;; body
    (check-to-win goals)))