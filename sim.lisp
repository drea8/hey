(defstruct point
  (x 0.0 :type float)
  (y 0.0 :type float))

(defstruct grid-cell
  (actors nil :type list)
  (cost 1.0 :type float))

(defstruct actor
  (id (gensym "ACTOR") :type symbol)
  (position (make-point) :type point)
  (glyph #\@ :type character))

(defstruct world-state
  (actors (make-hash-table) :type hash-table)
  (grid (make-hash-table :test #'equal) :type hash-table))



(defstruct state-monad
  (computation nil :type function))


;; not used currently
(defun return-state (value)
  (make-state-monad
   :computation (lambda (state)
                 (values value state))))

(defun bind-state (ma f)
  (make-state-monad
   :computation 
   (lambda (state)
     (multiple-value-bind (a new-state)
         (funcall (state-monad-computation ma) state)
       (funcall (state-monad-computation (funcall f a)) new-state)))))

(defmacro do-state (&body forms)
  (if (null forms)
      `(return-state nil)
      (let ((form (car forms)))
        (if (null (cdr forms))
            form
            `(bind-state ,form
                        (lambda (_)
                          (declare (ignore _))
                          (do-state ,@(cdr forms))))))))
;; ^ not used currently


(defun point-to-grid (point)
  (values (floor (point-x point)) (floor (point-y point))))

(defun get-cell (state x y)
  (or (gethash (cons x y) (world-state-grid state))
      (setf (gethash (cons x y) (world-state-grid state))
            (make-grid-cell))))

(defun update-actor-cell (state actor old-x old-y new-x new-y)
  (let ((old-cell (get-cell state old-x old-y))
        (new-cell (get-cell state new-x new-y)))
    (setf (grid-cell-actors old-cell)
          (remove actor (grid-cell-actors old-cell)))
    (push actor (grid-cell-actors new-cell))))


(defun move-actor (actor-id new-x new-y)
  (make-state-monad
   :computation
   (lambda (state)
     (let* ((actor (gethash actor-id (world-state-actors state)))
            (old-pos (actor-position actor))
            (new-pos (make-point :x (float new-x) :y (float new-y))))
       (multiple-value-bind (old-grid-x old-grid-y)
           (point-to-grid old-pos)
         (multiple-value-bind (new-grid-x new-grid-y)
             (point-to-grid new-pos)

           (setf (actor-position actor) new-pos)

           (update-actor-cell state actor 
                             old-grid-x old-grid-y
                             new-grid-x new-grid-y)
           (values t state)))))))



(defun cell-to-glyph (cell)
  (if (grid-cell-actors cell)
      (actor-glyph (car (grid-cell-actors cell)))
      #\.))

(defun display-grid (state min-x max-x min-y max-y)
  (loop for y from max-y downto min-y do
    (loop for x from min-x to max-x do
      (princ (cell-to-glyph (get-cell state x y))))
    (terpri)))


(defun make-sample-world ()
  (let ((state (make-world-state)))
    (let ((actor1 (make-actor :position (make-point :x 1.5 :y 1.5)))
	  (actor2 (make-actor :position (make-point :x 4.0 :y 5.0))))
      
      (setf (gethash (actor-id actor1) (world-state-actors state)) actor1)
      (setf (gethash (actor-id actor2) (world-state-actors state)) actor2)
      
      
      (multiple-value-bind (grid-x grid-y)
          (point-to-grid (actor-position actor1))
        (push actor1 (grid-cell-actors (get-cell state grid-x grid-y))))
      (multiple-value-bind (grid-x grid-y)
          (point-to-grid (actor-position actor2))
        (push actor2 (grid-cell-actors (get-cell state grid-x grid-y))))
      )
    state))

(defun sample-movement ()
  (let ((state (make-sample-world)))
    (format t "Initial state:~%")
    (display-grid state 0 10 0 10)
    
    (multiple-value-bind (result new-state)
        (funcall (state-monad-computation
                  (move-actor
		   (actor-id (car (grid-cell-actors 
                                   (get-cell state 1 1))))
                   2.5 2.5))
                 state)
      (declare (ignore result))
      
      (format t "~%After movement:~%")
      (display-grid new-state 0 10 0 10)))
  )
