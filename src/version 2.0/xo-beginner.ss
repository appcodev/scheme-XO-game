;; xo-beginner module
;; for xo-game version 2.0
;; Action, find the best path to win! (AI algorithm version 1.0 -- xo-beginner)

(module xo-beginner mzscheme
  ;; require
  (require (lib "list.ss")
           (lib "compat.ss")
           "xo-common-func.ss")
  
  ;; define functions
  
  ;; Heuristic
  (define (Heuristic state goalstate)
    (match state goalstate 0))
  
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
         (goals (GET-GOALSTATES player))
         (all-paths (all-possible-paths current-state goals)))
      
      ;; body
      ;; sort operator
      (define (better lst1 lst2)
        (< (car lst1)
           (car lst2)))
      
      (display "--- ")
      (display all-paths)
      
      (if (not (null? all-paths))
          ;; return the best path ...
          (cadr (reverse (car (sort better all-paths))))
          ;; if no-way to win choose one from state space
          )
      ))
  
  ;;> (require "xo-beginner.ss")
  ;;> (find-next-way '(O - - X O - X O -))
  ;;> (find-next-way '(- - - - O - X O -))
  
  (define (all-possible-paths state goals)
    (cond ((null? goals) '())
          (else
           (let ((path (a-tree state (car goals))))
             (display path)
             (if path
                 (cons path 
                       (all-possible-paths state (cdr goals)))
                 (all-possible-paths state (cdr goals)))
             ))))
  

  
  ;; provide (export functions)
  (provide whos-next)
  (provide find-next-way)
  (provide win?)
  
  
  )