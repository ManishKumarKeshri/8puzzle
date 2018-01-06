; 8 PUZZLE problem
; Name: Manish Kumar Keshri
; Date: 12/12/2017
; steps to run
; 1)start clisp
; 2) load the lisp file as (load "path\8puzzle.lisp")
; 3) call main function as (solution '#(1 2 3 4 5 6 7 8 E))

;functions:
; solution: main function to launch the 8 puzzle
; is solvable: to check feasibility
; find0: finds the loaction of emty state: helps in checking feasibilty
; inversion_count: finds the inversion count of the matrix: helps in checking the feasibilty
; sort states: to sort the puzzle ststes based on cost function
; partition: helps in sorting : Quick sort
; puzzle: function to start solving the puzzle
; display: to display the puzzle states
; move: to generate next move and put it into the queue
; isvisited: to check if the state has already been visited or not
; calculate_h: to calculate the heuristic

(defstruct state_node ;structure to store the attributes of the state
	state    ;array to store the puzzle elements
	state_id ;to store state id
	h ;heuristic value
	g ;path cost
	f ;cost function
)
(setf *all_state* (make-array 0 :fill-pointer t :adjustable t)) ;to store all the states generated during the traversal
(setf visited_states (make-array 0 :fill-pointer t :adjustable t)) ; to store the states that have beeen visited
(setf solvable_flag 0) ;solvable flag to check the feasibility of the given problem
(setf intial_state (make-array 9)) ;the given puzzle state
(defun solution(start_state) ;Main function to run the script and intailise
	(loop for i from 0 to 8 do 
		(if (eq 'E (aref start_state i))
			(setf (aref intial_state i) 0)		
		(setf (aref intial_state i) (aref start_state i))
		)		
	)
	(setf solvable_flag (is_solvable intial_state)) ;calling the function to check solvability
	(if (/= 0 solvable_flag) ; condition check for solavability
		(progn 
			(setf current_state intial_state)
			(vector-push-extend (make-state_node
				:state current_state
				:state_id (length *all_state*)
				:h (calculate_h current_state) ; function called to calculate heuristic
				:g 0
				:f (calculate_h current_state)) *all_state*)
			(setf goal_state '#(1 2 3 4 5 6 7 8 0)) ; setting the goal state
			(puzzle) ;function to start solving the puzzle
			(setf expanded_nodes (-(length visited_states) 1))
			(format t "~%Nodes expanded: ~S" expanded_nodes)
		)
		(progn
			(format t "~%Infeasible puzzle")
		)
	)
)
(defun is_solvable(check_state) ; check solvability of the puzzle
	(setf inv_count (inversion_count check_state))
			(if (oddp inv_count) ; to check if the inversion pair count is odd
				0
				1
			)	
)
(defun find0(check_state) ; find the position of empty spot in the given puzzle
	(setf N (sqrt(length check_state)))
	(loop for i from (1- (length check_state)) downto 0 do
		(if (= 0 (aref check_state i))
			(mod i N)
		)
	)
)
(defun inversion_count(new_state) ; find the umber of inversion count
	(setf inv_count 0)
	(loop for i from 0 to (1- (1- (length new_state))) do
		(loop for j from (+ i 1) to (1- (length new_state)) do
			(if (and (/= 0 (aref new_state j)) (/= 0 (aref new_state i)) (> (aref new_state i)(aref new_state j) ))
				(incf inv_count) ; increment count of inverse pairs
			)
		)
	)
	inv_count
)
(defun sort_states (low high) ;sort the states based on quick sort
	(if (< low high)
		(progn
			(setf p (partition low high))
			(sort_states low (- p 1))
			(sort_states (+ p 1) high)
		)
	)	
)
(defun partition (low high) ;paritioning to help in quick sort
	(setf pivot (state_node-f (aref *all_state* high)))
	(setf i (- low 1))
	(loop for j from low to (- high 1) do
		(if (>= (state_node-f (aref *all_state* j)) pivot)
			(progn
				(incf i)
				(rotatef (aref *all_state* i) (aref *all_state* j))
			)
		)
	)
	(rotatef (aref *all_state* (+ 1 i)) (aref *all_state* high))
	(+ i 1)
)
(defun puzzle() ; to solve the puzzle
	(sort_states 0 (1- (length *all_state*))) ;sort the states
	(setq current_state (vector-pop *all_state*)) ;define current state
	(setf copy_s (copy-seq (state_node-state current_state))) ;make a copy of current state
	(let ((temp_state (copy-seq (state_node-state current_state))))
		;(format t "~S" (state_node-g current_state))
		(display temp_state)
		(vector-push-extend temp_state visited_states)
		(if (equalp goal_state temp_state)
			(progn
				(format t "~% Goal state reached")
			)
			(progn
				(loop for i from 0 to 8 do
					(if (= 0 (aref (state_node-state current_state) i))
						(progn
								(if (and (/= i 2) (/= i 5) (/= i 8))  
									(move current_state i (+ i 1)) ; function call to generate new states from current states
								)
								(if (and (/= i 6) (/= i 7) (/= i 8))
									(move current_state i (+ i 3))
								)
								(if (and (/= i 0) (/= i 3) (/= i 6))
									(move current_state i (- i 1))
								)
								(if (and (/= i 0) (/= i 1) (/= i 2))
									(move current_state i (- i 3))
								)
						)
					)
				)
				(puzzle) ; recursive function call until the goal state is reached
			)
		)
	)
)

(defun display(current_state) ; display the contents of the current puzzle state
	(setf sqrt_size(sqrt (length current_state)))
	(format t "~%~%")
	(loop for i from 0 to (- (length current_state) 1) do
		(if (or (= 3 i) (= 6 i))
			(format t "~%")
		)
		(if (or (= 0 i) (= 3 i) (= 6 i))
			(format t "|")
		)
	(if (= 0 (aref current_state i))
		(format t "E|")
 		(format t "~a|" (aref current_state i))
	)
	)
)
(defun move(move_state empty_spot block) ; to get the next move of the puzzle
	(setf temp_state (copy-seq (state_node-state move_state)))
	(rotatef (aref temp_state empty_spot) (aref temp_state block))
	(if (= 1 (isvisited temp_state))
		(vector-push-extend(make-state_node
					:state temp_state
					:state_id (length *all_state*)
					:g (+ 0.1 (state_node-g move_state))
					:h (calculate_h temp_state)
					:f (+ (+ 0.1 (state_node-g move_state)) (calculate_h temp_state)))
					*all_state*
		)	
	)
)
(defun isvisited(temp_state) ; to check if the given state is already visited or not
	(loop for i from 0 to (1- (length visited_states)) do
		(if (equalp temp_state (aref visited_states i))
			(return-from isvisited 0)	
		)
	)
	1
)

(defun calculate_h(to_calculate) ; to calculate the heuristic
	(setq hr 0)
	(loop for i from 0 to 8 do
		(if (/= (rem (+ 1 i) 9) (aref to_calculate i))
					(incf hr) ; increment heuristic for every misplaced tile
		)
	)
	hr
)

