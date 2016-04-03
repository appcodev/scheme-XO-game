;; XO common functions
;; - GET-MOVES
;; - Goal-States

(module xo-common-func mzscheme
  ;; require
  
  ;; define variables
  ;; version 1.0
  ;; state space :: Get-MOVES
  ;; define avaible is '- to check the available space 
  (define available '-)
  (define P1 'O)
  (define P2 'X)
  
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
  
  ;; define functions
  
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
  

  ;; ------------------ Goal State -------------------------
  ;; get all goal state of X/O
  (define (GET-GOALSTATES x)
    (cadr (assoc x Goal)))
  
  ;; ---------------- End Goal State -----------------------
  
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
  
  ;; win? 
  ;; this function used to check current player is the winner or not
  (define (win? path player)
    (let ((goals (GET-GOALSTATES player)))
      ;; define private function
      (define (check-to-win all-goals)
        (cond ((null? all-goals) #f)
              ((and (number? path)(= path 0)) #f)
              ((match? path (car all-goals)) #t)
              (else
               (check-to-win (cdr all-goals)))))
      
      ;; body
      (check-to-win goals)))
  
  ;; provide
  (provide available)
  (provide whos-next)
  (provide match)
  (provide match?)
  (provide win?)
  (provide GET-GOALSTATES)
  (provide GET-MOVES)
  
  )