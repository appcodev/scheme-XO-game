;; XO game
(require (lib "list.ss"))
(require (lib "compat.ss"))

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

;; --- GET-MOVES

;; simple decision put X or O
;; 9 - (number of -)
;; if odd then put 'who second  => 'X
;; otherwise   put 'who first   => 'O

(define (GET-MOVES state)
  (if (= (modulo (num-space state) 2) 1)
      (get-moves state 'O)
      (get-moves state 'X)))

(define (num-space state)
  (cond ((null? state) 0)
        ((eq? (car state) available)
         (+ 1 (num-space (cdr state)))) 
        (else 
         (num-space (cdr state)))))

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

;; ------ End GET-MOVES

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
  (cond ((null? state1) 0)
        ((not (eq? (car state1) available))
         (if (eq? (car state1) (car goal))
             (+ (+ count 1) 
                (match (cdr state1) (cdr goal) (+ count 1)))
             (match (cdr state1) (cdr goal) 0)))   
        (else
         (match (cdr state1) (cdr goal) 0))))

;; return #t/#f
(define (match? state goalstate)
  (= (match state goalstate 0) 6))


;; Searching by A*
;; Algorthm A Implementation
(define (a-tree startState goalState)

  (define (a-paths paths)
    (cond ((null? paths) #f)
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

