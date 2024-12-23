(defparameter *size* 15)
(defparameter *field* (make-array (* *size* *size*) :initial-element nil))
(defparameter *default-game* '(
                               ; first row
                               (3 . 0)

                               ; second row
                               (2 . 1)
                               (4 . 1)

                               ; third row
                               (1 . 2)
                               (2 . 2)
                               (3 . 2)
                               (4 . 2)
                               (5 . 2)
                               
                               ; fourth row 
                               (0 . 3)
                               (1 . 3)
                               (2 . 3)
                               (3 . 3)
                               (4 . 3)
                               (5 . 3)
                               (6 . 3)

                               ))

;; Game of life stuff

(defun print-field ()
  "Prints the whole field to the standard output"
  (loop for i from 0 below (* *size* *size*) do 
        (if (null (elt *field* i)) 
            (format t " ")
            (format t "#"))
        (if (and 
              (/= i 0) 
              (= (mod i *size*) (- *size* 1)))
            (format t "~A" #\newline))))

(defun get-cell-position (x y)
  "Convert a cell's coordinates (x, y) into a vector index"
  (if (or (> x 14) (> y 14))
          (error "One of the values (x or y) is out of bounds"))
  (+ (* y *size*) x))

(defun get-cell-value (x y)
  "Get a cell's value"
  (elt *field* (get-cell-position x y)))

(defun set-cell-position (x y value)
  "Set a cell at coordinates (x, y) to the value provided (only nil and t can be used)"
  (if (not (or (null value) (eq value t)))
      (error "Value has to be either nil or t"))
  (setf (elt *field* (get-cell-position x y)) value))

(defun default-game-init ()
  "Import the default game setup into the field"
  (loop for i in *default-game* do
        (set-cell-position (+ (car i) 4) (+ (cdr i) 5) t)))

(defun next-step-collect-values (x y)
  "Return the amount of neighboring cells near a cell in the field"
  (let ((accumulated 0))
    (loop for i from -1 to 1 do
          (loop for j from -1 to 1 do
                (if (and (= i 0) (= j 0)) ()
                      (handler-case (let ((value (get-cell-value (+ x j) (+ y i))))
                                       (when (eq value t) (incf accumulated)))
                        (error (c) ())))))
    accumulated))

(defun next-step-new-cell (x y)
  "Returns a value of a new cell based on Conway's game of life rules"
  (let ((old-value (get-cell-value x y))
        (next-acc (next-step-collect-values x y)))
    (cond ((and old-value (< next-acc 2)) nil)
          ((and old-value (>= next-acc 2) (< next-acc 4)) t)
          ((and old-value (> next-acc 3)) nil) 
          ((and (null old-value) (= next-acc 3)) t) )))

(defun next-step ()
  "Apply the rules of Conway's game of life to the field and change it to the next state"
  (let ((new-field (make-array (* *size* *size*) :initial-element nil)))
    (loop for x from 0 below *size* do 
          (loop for y from 0 below *size* do
                (setf 
                  (elt new-field (get-cell-position x y)) 
                  (next-step-new-cell x y))))
    (setf *field* new-field)))

;; Server stuff
(ql:quickload "usocket")

(defun field-to-cons () 
  "Converts a field into a list of cons pairs, where each pair contains the coordinates of an alive cell"
  (loop for i from 0 below (* *size* *size*) 
        for x = (mod i *size*)
        for y = (floor (/ (float i) (float *size*)))
        if (eq (get-cell-value x y) t)
        collect (cons x y)))

;; Mainloop 

(defun main () 
  "Main function"
  (setf *field* (make-array (* *size* *size*) :initial-element nil))
  (default-game-init)
  (let* ((socket (usocket:socket-listen "127.0.0.1" 5500))
         (connection (usocket:socket-accept socket :element-type 'character)))
    (unwind-protect 
        (progn 
          (format t "==> Start~A" #\newline)
          (loop do 
           (format (usocket:socket-stream connection) "~A" (field-to-cons)) 
           (force-output (usocket:socket-stream connection)) 
           (format t "=> State sent~%") 
           (next-step) 
           (sleep 0.5)
           ))
      (progn
        (format t "==> Closing socket~%")
        (usocket:socket-close connection)
        (usocket:socket-close socket)))))
