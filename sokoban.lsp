(defun reload ()
  (load "sokoban.lsp")
  )
  
(defun load-a-star ()
  (load "a-star.lsp"))
  
(defun reload-all ()
  (reload)
  (load-a-star)
  )

(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )
  
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )
  
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

;Helper function of goal-test-row. 
;This checks whether "box"(2) exists or not column by column in one row.
;If goal state, then it should be "box+goal"(5) instead of "box"(2).
;Thus, if there is any "box"(2), it means that the state is not a goal.
;It returns true when state is NOT a goal state. It returns NIL when there's no "box"(2) in this row.(Although, it doesn't mean that the state is a goal state)
(defun goal-test-col (s)
	(cond	((null s)	nil)
		(t	(if (isBox (car s))	;if found any "box"(2), return true since it is not a goal state
			t
			(goal-test-col (cdr s))	;otherwise, keep recurse on itself
			);end if
		);end t
	);end cond
);end defun

;Helper function of goal-test.
;This checks whether "box"(2) exists or not row by row in one given state.
;For each row, we use previously defined function "goal-test-col" to check existence of "box"(2)
;It returns true when state is NOT a goal state. It returns NIL when there's no "box"(2) in this state.(Now it means that the state is a goal state)
;As mentioned above, it returns reversed results of goal test so that we need final function to reverse the return value back.
(defun goal-test-row (s)
	(cond	((null s)	nil)
		(t	(let 	((x (goal-test-col (car s))))
					(if x	;if there's any box which is not placed in the goal, return true
					t
					(goal-test-row (cdr s))	;otherwise, keep recurse on itself
					);end if
			);end let
		);end t
	);end cond
);end defun

;Basically, goal-test-row function returns reverse value of what it's supposed to return. Thus, reverse back
(defun goal-test (s)
	(cond	((null s)	nil)
		((goal-test-row s)	nil)	;if goal-test-row is true, reverses the result and returns NIL
		(t	t)	;otherwise, returns true
	);end cond
);end defun

;Helper function of get-square. It traces the value column by column in a row
(defun get-square-helper (s c)	;here, s is just list
	(cond	((null s)	1)	;if the square is outside the scope of the problem, return the value of a wall
		((> c 0)	(get-square-helper (cdr s) (- c 1)))
		(t	(car s))
	)
)

;It traces the value row by row and then use helper function above to return value
(defun get-square (s r c)	;here, s is list of lists
	(cond	((null s)	1)	;if the square is outside the scope of the problem, return the value of a wall
		((> r 0)	(get-square (cdr s) (- r 1) c))
		(t	(get-square-helper (car s) c))
	)
)

;Helper function of set-square. It approaches the target column by column and change the value
;Returns a list with changed value in target position
(defun set-col (s c v)	;here, s is just list(row)
	(cond	((null s)	nil)
		((> c 0)	(cons (car s) (set-col (cdr s) (- c 1) v)))
		(t	(cons v (cdr s)))
	)
)

;It approcahes the target row by row first and then use helper function to change value in target position
;Returns a new state
(defun set-square (s r c v)
	(cond	((null s)	nil)
		((> r 0)	(cons (car s) (set-square (cdr s) (- r 1) c v)))
		(t	(cons (set-col (car s) c v) (cdr s)))
	)
)

;Helper function of try-move
;DIRECTION: UP=1 DOWN=2 LEFT=3 RIGHT=4
;It checks validity first, then returns resulted state (if invalid, returns NIL)
;For each direction, there are total 12 possible valid cases:
;if Keeper's position is "Keeper"(3) or "Keeper+goal"(6)
;if next position in given direction is "Blank"(0) or "Box"(2) or "Goal"(4) or "Box+goal"(5)
;if "Box"(2), the next position only can be "Blank"(0) or "Goal"(4)
;if "Box+goal"(5), the next position only can be "Blank"(0) or "Goal"(4) again
;if "Blank"(0) or "Goal"(4), the next position doesn't matter
(defun try-move (s keeper-col keeper-row d)
	(cond	((null s)	nil)
		((and (= d 1) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (- keeper-row 1) keeper-col) 0))	(set-square (set-square s keeper-row keeper-col 0) (- keeper-row 1) keeper-col 3))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (- keeper-row 1) keeper-col) 2) (= (get-square s (- keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) (- keeper-row 1) keeper-col 3) (- keeper-row 2) keeper-col 2))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (- keeper-row 1) keeper-col) 2) (= (get-square s (- keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) (- keeper-row 1) keeper-col 3) (- keeper-row 2) keeper-col 5))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (- keeper-row 1) keeper-col) 4))	(set-square (set-square s keeper-row keeper-col 0) (- keeper-row 1) keeper-col 6))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (- keeper-row 1) keeper-col) 5) (= (get-square s (- keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) (- keeper-row 1) keeper-col 6) (- keeper-row 2) keeper-col 2))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (- keeper-row 1) keeper-col) 5) (= (get-square s (- keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) (- keeper-row 1) keeper-col 6) (- keeper-row 2) keeper-col 5))

		((and (= d 1) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (- keeper-row 1) keeper-col) 0))	(set-square (set-square s keeper-row keeper-col 4) (- keeper-row 1) keeper-col 3))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (- keeper-row 1) keeper-col) 2) (= (get-square s (- keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) (- keeper-row 1) keeper-col 3) (- keeper-row 2) keeper-col 2))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (- keeper-row 1) keeper-col) 2) (= (get-square s (- keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) (- keeper-row 1) keeper-col 3) (- keeper-row 2) keeper-col 5))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (- keeper-row 1) keeper-col) 4))	(set-square (set-square s keeper-row keeper-col 4) (- keeper-row 1) keeper-col 6))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (- keeper-row 1) keeper-col) 5) (= (get-square s (- keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) (- keeper-row 1) keeper-col 6) (- keeper-row 2) keeper-col 2))
		((and (= d 1) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (- keeper-row 1) keeper-col) 5) (= (get-square s (- keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) (- keeper-row 1) keeper-col 6) (- keeper-row 2) keeper-col 5))

		((and (= d 2) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (+ keeper-row 1) keeper-col) 0))	(set-square (set-square s keeper-row keeper-col 0) (+ keeper-row 1) keeper-col 3))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (+ keeper-row 1) keeper-col) 2) (= (get-square s (+ keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) (+ keeper-row 1) keeper-col 3) (+ keeper-row 2) keeper-col 2))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (+ keeper-row 1) keeper-col) 2) (= (get-square s (+ keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) (+ keeper-row 1) keeper-col 3) (+ keeper-row 2) keeper-col 5))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (+ keeper-row 1) keeper-col) 4))	(set-square (set-square s keeper-row keeper-col 0) (+ keeper-row 1) keeper-col 6))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (+ keeper-row 1) keeper-col) 5) (= (get-square s (+ keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) (+ keeper-row 1) keeper-col 6) (+ keeper-row 2) keeper-col 2))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 3) (= (get-square s (+ keeper-row 1) keeper-col) 5) (= (get-square s (+ keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) (+ keeper-row 1) keeper-col 6) (+ keeper-row 2) keeper-col 5))

		((and (= d 2) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (+ keeper-row 1) keeper-col) 0))	(set-square (set-square s keeper-row keeper-col 4) (+ keeper-row 1) keeper-col 3))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (+ keeper-row 1) keeper-col) 2) (= (get-square s (+ keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) (+ keeper-row 1) keeper-col 3) (+ keeper-row 2) keeper-col 2))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (+ keeper-row 1) keeper-col) 2) (= (get-square s (+ keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) (+ keeper-row 1) keeper-col 3) (+ keeper-row 2) keeper-col 5))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (+ keeper-row 1) keeper-col) 4))	(set-square (set-square s keeper-row keeper-col 4) (+ keeper-row 1) keeper-col 6))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (+ keeper-row 1) keeper-col) 5) (= (get-square s (+ keeper-row 2) keeper-col) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) (+ keeper-row 1) keeper-col 6) (+ keeper-row 2) keeper-col 2))
		((and (= d 2) (= (get-square s keeper-row keeper-col) 6) (= (get-square s (+ keeper-row 1) keeper-col) 5) (= (get-square s (+ keeper-row 2) keeper-col) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) (+ keeper-row 1) keeper-col 6) (+ keeper-row 2) keeper-col 5))

		((and (= d 3) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (- keeper-col 1)) 0))	(set-square (set-square s keeper-row keeper-col 0) keeper-row (- keeper-col 1) 3))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (- keeper-col 1)) 2) (= (get-square s keeper-row (- keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (- keeper-col 1) 3) keeper-row (- keeper-col 2) 2))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (- keeper-col 1)) 2) (= (get-square s keeper-row (- keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (- keeper-col 1) 3) keeper-row (- keeper-col 2) 5))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (- keeper-col 1)) 4))	(set-square (set-square s keeper-row keeper-col 0) keeper-row (- keeper-col 1) 6))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (- keeper-col 1)) 5) (= (get-square s keeper-row (- keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (- keeper-col 1) 6) keeper-row (- keeper-col 2) 2))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (- keeper-col 1)) 5) (= (get-square s keeper-row (- keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (- keeper-col 1) 6) keeper-row (- keeper-col 2) 5))

		((and (= d 3) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (- keeper-col 1)) 0))	(set-square (set-square s keeper-row keeper-col 4) keeper-row (- keeper-col 1) 3))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (- keeper-col 1)) 2) (= (get-square s keeper-row (- keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (- keeper-col 1) 3) keeper-row (- keeper-col 2) 2))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (- keeper-col 1)) 2) (= (get-square s keeper-row (- keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (- keeper-col 1) 3) keeper-row (- keeper-col 2) 5))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (- keeper-col 1)) 4))	(set-square (set-square s keeper-row keeper-col 4) keeper-row (- keeper-col 1) 6))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (- keeper-col 1)) 5) (= (get-square s keeper-row (- keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (- keeper-col 1) 6) keeper-row (- keeper-col 2) 2))
		((and (= d 3) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (- keeper-col 1)) 5) (= (get-square s keeper-row (- keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (- keeper-col 1) 6) keeper-row (- keeper-col 2) 5))

		((and (= d 4) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (+ keeper-col 1)) 0))	(set-square (set-square s keeper-row keeper-col 0) keeper-row (+ keeper-col 1) 3))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (+ keeper-col 1)) 2) (= (get-square s keeper-row (+ keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (+ keeper-col 1) 3) keeper-row (+ keeper-col 2) 2))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (+ keeper-col 1)) 2) (= (get-square s keeper-row (+ keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (+ keeper-col 1) 3) keeper-row (+ keeper-col 2) 5))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (+ keeper-col 1)) 4))	(set-square (set-square s keeper-row keeper-col 0) keeper-row (+ keeper-col 1) 6))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (+ keeper-col 1)) 5) (= (get-square s keeper-row (+ keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (+ keeper-col 1) 6) keeper-row (+ keeper-col 2) 2))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 3) (= (get-square s keeper-row (+ keeper-col 1)) 5) (= (get-square s keeper-row (+ keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 0) keeper-row (+ keeper-col 1) 6) keeper-row (+ keeper-col 2) 5))

		((and (= d 4) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (+ keeper-col 1)) 0))	(set-square (set-square s keeper-row keeper-col 4) keeper-row (+ keeper-col 1) 3))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (+ keeper-col 1)) 2) (= (get-square s keeper-row (+ keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (+ keeper-col 1) 3) keeper-row (+ keeper-col 2) 2))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (+ keeper-col 1)) 2) (= (get-square s keeper-row (+ keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (+ keeper-col 1) 3) keeper-row (+ keeper-col 2) 5))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (+ keeper-col 1)) 4))	(set-square (set-square s keeper-row keeper-col 4) keeper-row (+ keeper-col 1) 6))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (+ keeper-col 1)) 5) (= (get-square s keeper-row (+ keeper-col 2)) 0))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (+ keeper-col 1) 6) keeper-row (+ keeper-col 2) 2))
		((and (= d 4) (= (get-square s keeper-row keeper-col) 6) (= (get-square s keeper-row (+ keeper-col 1)) 5) (= (get-square s keeper-row (+ keeper-col 2)) 4))	(set-square (set-square (set-square s keeper-row keeper-col 4) keeper-row (+ keeper-col 1) 6) keeper-row (+ keeper-col 2) 5))

		(t	nil)	;otherwise, returns NIL
	)
)

;Returns the list of all states that can be reached from the given state in one move
;First, get the position of keeper
;Then, put those value with state and direction into helper function above
;DIRECTION: UP=1 DOWN=2 LEFT=3 RIGHT=4
;Combine them all into one list and then use cleanUpList in order to delete NIL in the list
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result	(append (list (try-move s x y 1)) (list (try-move s x y 4)) (list (try-move s x y 2)) (list (try-move s x y 3))))
	 )
    (cleanUpList result);end
   );end let
  );

;Just return 0
(defun h0 (s)
	0
  )

;Helper function of h1
;Investigate column by column in a row and add 1 if there is any
(defun h1-help (s);here, s is just list
	(cond	((null s)	0)
		((isBox (car s))	(+ 1 (h1-help (cdr s))))
		(t	(h1-help (cdr s)))
	);end cond
);end defun

;It is admissible because to meet a goal state, all boxes should be placed in goal positions
;which means each of boxes which are not on goal position should be moved to a goal position with at least one move
;so that number of boxes which are not on goal positions never exceeds real minimum moves to a goal-state
;This function investigates row by row in a given state and for each row, uses helper function above and find number of boxes and returns sum
(defun h1 (s)
	(cond	((null s)	0)
		(t	(+ (h1-help (car s)) (h1 (cdr s))))
	);end cond
);end defun

;Basically I am going to use Manhattan distance between each box and its nearest goal position.
;As below, I break down into several small helper fucntions to be used in my own heuristic function.

;This function below is finding Manhattan distance between two coordinates
(defun manhattan-distance (box goal) ;box and goal are coordinates and it returns number
	(cond	((null box)	0)	
			((> (car box) (car goal))	(+ (- (car box) (car goal)) (manhattan-distance (cdr box) (cdr goal)))) ;break into two cases in order to avoid getting negative value while minus computation
			(t	(+ (- (car goal) (car box)) (manhattan-distance (cdr box) (cdr goal))))
	)
)

;For each box, it figures out all possible manhattan distances to all goal positions; which means it returns list of number
(defun possible-distance-each-box (box goal-list) ;box is a coordinate and goal is a list of coordinates
	(cond	((null goal-list)	nil)
			(t	(cons (manhattan-distance box (car goal-list)) (possible-distance-each-box box (cdr goal-list)))))
)

;For every boxes in list, uses possible-distance-each-box function above, and returns all possible distances
(defun possible-distance (box-list goal-list) ;returns list of lists of number
	(cond	((null box-list)	nil)
			(t	(cons (possible-distance-each-box (car box-list) goal-list) (possible-distance (cdr box-list) goal-list)))
	)
)

;find minimum number in the list
(defun find-min (s) ;s is just list
	(cond	((null (cdr s))	(car s)) ;if last element, just returns itself
			((< (car s)	(find-min (cdr s))) (car s)) ;compare each element and return the smaller one
			(t	(find-min (cdr s)))
	)
)

;accepts list of lists and returns minimum numbers of each list
(defun list-min (s) ; s is list of lists
	(cond	((null s)	nil)
			(t	(cons (find-min (car s)) (list-min (cdr s))))
	)
)

;calculates sum of all numbers in the list
(defun sum-list (s)
	(cond	((null s)	0)
			(t	(+ (car s) (sum-list (cdr s))))
	)
)

;with given list of columns and number of row, make it into list of coordinates
(defun make-coordinate (cols row) ;cols is list and row is number
	(cond	((null cols)	nil)
			(t	(cons (cons (car cols) (list row)) (make-coordinate (cdr cols) row)))
	)
)

;Helper function of getBoxPositions. It investigates column by column in a row, and saves all the column numbers into list if there's box
(defun getBoxColumns (r col)
	(cond ((null r) nil)
		(t 	(if 	(or (isBox (car r)) (isBoxStar (car r)))
			(cons col (getBoxColumns (cdr r) (+ col 1)))
			(getBoxColumns (cdr r) (+ col 1))
			);end if
		);end t
	);end cond
)
;It investigates row by row with using helper function above to get column numbers of boxes. Then it puts column and row number together into coordinates and returns list of all the box coordinates
(defun getBoxPositions (s row) ;returns list of (column, row)
	(cond	((null s) nil)
			(t	(let ((cols (getBoxColumns (car s) 0)))
					(if cols
					;box is in this row
					(append (make-coordinate cols row) (getBoxPositions (cdr s) (+ row 1)))
					;otherwise move on
					(getBoxPositions (cdr s) (+ row 1))
					);end if
				);end let
			);end t
	);end cond
);end defun

;Exactly same as getBoxColumns and getBoxPositions above except its target is goal
(defun getGoalColumns (r col)
	(cond ((null r) nil)
		(t 	(if 	(or (isStar (car r)) (isKeeperStar (car r)))
			(cons col (getGoalColumns (cdr r) (+ col 1)))
			(getGoalColumns (cdr r) (+ col 1))
			);end if
		);end t
	);end cond
)
(defun getGoalPositions (s row) ;returns list of (column, row)
	(cond	((null s) nil)
			(t	(let ((cols (getGoalColumns (car s) 0)))
					(if cols
					;box is in this row
					(append (make-coordinate cols row) (getGoalPositions (cdr s) (+ row 1)))
					;otherwise move on
					(getGoalPositions (cdr s) (+ row 1))
					);end if
				);end let
			);end t
	);end cond
);end defun

;my own heuristic function based on using minimum available manhattan distances for every boxes to the nearest goal
;it first gets all the boxex and goals positions
;then use possible-distance so that figure out list of manhattan distances
;then use mins to pick only one minimum distance for each box
;last, add all the minimum manhattan distances for each box
;to make sure not include NIL in the list, add cleanUpList
(defun heuristic (s)
	(let* ((box-pos (getBoxPositions s 0))
	 (goal-pos (getGoalPositions s 0))
	 (dist	(cleanUpList (possible-distance box-pos goal-pos)))
	 (mins	(cleanUpList (list-min dist)))
	 (result (sum-list mins)))
	 
	 result
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq s1 '( (1 1 1 1 1)
(1 0 0 4 1)
(1 0 2 0 1)
(1 0 3 0 1)
(1 0 0 0 1)
(1 1 1 1 1)
))
(setq s2 '((1 1 1 1 1)
(1 0 0 4 1)
(1 0 2 3 1)
(1 0 0 0 1)
(1 0 0 0 1)
(1 1 1 1 1)
))
(setq s3 '((1 1 1 1 1)
(1 0 0 6 1)
(1 0 2 0 1)
(1 0 0 0 1)
(1 0 0 0 1)
(1 1 1 1 1)
))
(setq s4 '((1 1 1 1 1)
(1 4 2 0 1)
(1 0 0 0 1)
(1 0 0 0 1)
(1 0 5 3 1)
(1 1 1 1 1)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
