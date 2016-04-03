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
    (match state goalstate))
  
  ;; match?
  ;; return #t/#f
  (define (match? state goalstate)
    (= (match state goalstate) 3))
  
  ;; count number of match
  (define (match c_state c_goal)
    ;; recursive match function
    (define (match-loop state1 goal count)
      (cond ((null? state1) count)
            ((eq? (car state1) available)
             (match-loop (cdr state1) (cdr goal) count))
            ((not (eq? (car goal) available))
             (if (eq? (car state1) (car goal)) 
                 (match-loop (cdr state1) (cdr goal) (+ count 1))
                 ;; if has some opponent in goal return 0 score
                 0))   
            (else
             (match-loop (cdr state1) (cdr goal) count))))
  ;;body  
  (match-loop c_state c_goal 0))
  
  ;; Searching by A*
  ;; Algorthm A Implementation
  (define (a-tree startState goalState)
    
    (define (a-paths paths)
      (cond ((null? paths) #f)
            ((<= (match (cadar paths) goalState) 0) #f)
            
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
      
      (display all-paths)
      (if (not (null? all-paths))
          ;; return the best path ...
          (cadr (reverse (car (sort better all-paths))))
          ;; player surrender from this game
          '())
      ))
  
  (define (all-possible-paths state goals)
    (cond ((null? goals) '())
          (else
           (let ((path (a-tree state (car goals))))
             (if path
                 (cons path 
                       (all-possible-paths state (cdr goals)))
                 (all-possible-paths state (cdr goals)))
             ))))
  

  
  ;; provide (export functions)
  (provide whos-next)
  (provide find-next-way)
  (provide match)
  (provide match?)
  (provide GET-GOALSTATES)
  
  )