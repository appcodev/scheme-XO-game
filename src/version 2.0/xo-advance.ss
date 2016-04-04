;; XO Advance AI
;; AI version 2.0

(module xo-advance mzscheme
   ;; require
  (require (lib "list.ss")
           (lib "compat.ss")
           "xo-common-func.ss")
  
  ;; define functions
  
  ;; Heuristic
  ;; if match goal + 100
  ;; if can't go to goal -1 for each path
  (define (Heuristic state goalstate)
    (let ((h (match state goalstate)))
      (if (= h 3) (+ 110 h) h)))
  
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
                 ;; if has some opponent in goal return -1 score
                 -1))   
            (else
             (match-loop (cdr state1) (cdr goal) count))))
  ;;body  
  (match-loop c_state c_goal 0))
  
  ;; Searching by A*
  ;; Algorthm A Implementation
  (define (a-tree startState goalState)
    
    (define (a-paths paths)
      ;(display paths)
      ;(newline)
      (cond ((null? paths) #f)
            ((and (> (length paths) 1)
                  (< (match (cadar paths) goalState) 0)) #f)
            
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
         (op-cindex (opponent-critical-index current-state player))
         (all-paths (all-possible-paths current-state goals)))
      
      ;; body
      ;; sort operator :: number of move (cost)
      (define (better lst1 lst2)
        (< (car lst1)
           (car lst2)))
      
      (if (not (null? all-paths))
          ;; return the best path ...
          (let* ((best-path (cadr (reverse (car (sort better all-paths)))))
                 ;; check sum huristic of best-path
                 (bp-h (sum-heuristic best-path goals)))
            
            ;; if bp-h >= 100 choose best-path
            ;; if op-cindex >= 0 choose (replace current-state op-cindex player) path
            ;; else choose best-path
            (cond ((>= bp-h 100) best-path)
                  ((>= op-cindex 0) (replace current-state op-cindex player))
                  (else
                   best-path))
            )
          ;; [else] 
          ;; if all-paths is null = no way for win
          ;; player choose defend or random walk
          (if (>= op-cindex 0)
              ;; defend
              (replace current-state op-cindex player)
              ;; random walk? choose first path*
              (caar (GET-MOVES current-state)) ))
      ))
  
  (define (all-possible-paths state goals)
    (cond ((null? goals) '())
          (else
           (let ((path (a-tree state (car goals))))
             ;(display path)
             ;(display "->")
             ;(display (car goals))
             ;(newline)
             (if path
                 (cons path 
                       (all-possible-paths state (cdr goals)))
                 (all-possible-paths state (cdr goals)))
             ))))
  
  ;; --------------------- Defend Decision --------------------
  ;; Defend
  ;; check oppotunity for opponent get win
  ;; return critical index
  (define (opponent-critical-index state player)
    (let* ((op (opponent player))
           (op-all-states (get-moves state op))
           (op-goals (GET-GOALSTATES op))
           (op-h-list (list-all-state op-all-states op-goals)))
      
      ;; get state that has the most heuristic value 
      (if (not (null? op-h-list))
          (let* ((op-best-h (list-ref (sort better-h op-h-list) 0)))
            ;; heuristic must be more than or equal 100
            (if (>= (car op-best-h) 100)
                ;; result and compare position with current state
                ;; for response critical index
                (critical-index state (cadr op-best-h))
                -1)
            ))
      ))
  
  ;; summation of heuristic of all state
  (define (sum-heuristic state goals)
    (cond ((null? goals) 0)
          (else
           (+ (Heuristic state (car goals))
             (sum-heuristic state (cdr goals))))))
  
  ;; all state and hueristic value of each state
  (define (list-all-state all-states goals)
    (cond ((null? all-states) '())
          (else
           (let ((state (caar all-states)))
             (cons (list (sum-heuristic state goals) state)
                   (list-all-state (cdr all-states) goals))
             ))))
  
  ;; sort operator
  (define (better-h lst1 lst2)
    (> (car lst1) (car lst2)))
  
  ;; report critical index
  (define (critical-index current-state op-win-state)
    ;; loop
    (define (critical-loop state op-state index)
      (cond ((null? state) -1)
             ((eq? (car state) (car op-state))
              (critical-loop (cdr state) (cdr op-state) (+ index 1)))
             (else
              index)))
    
    (critical-loop current-state op-win-state 0))
  
  ;; --------------------- End Defend Decision --------------------
  
  ;; provide (export functions)
  (provide whos-next)
  (provide find-next-way)
  (provide match)
  (provide match?)
  (provide GET-GOALSTATES)
  
  ;; for display
  (provide display-table)
  
  ;; for test function
  (provide opponent-critical-index)
  (provide sum-heuristic)
  (provide list-all-state)
  (provide a-tree)
  )