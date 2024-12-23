(ql:quickload "usocket")

; Client stuff

(defparameter *size* 15)
(defparameter *field* (make-array (* *size* *size*) :initial-element nil))
(defparameter *cells* nil)

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

(defun field-init ()
  "Import the default game setup into the field"
  (loop for i in *cells* do
        (set-cell-position (car i) (cdr i) t)))

(defun load-state (s)
  "Loads a state from the stream"
  (setf *cells* (read s)))

(defun clear-field ()
  "Loads the field from the state"
  (setf *field* (make-array (* *size* *size*) :initial-element nil)))

;; Mainloop

(defun main ()
  "Main function"
  (let ((socket (usocket:socket-connect "127.0.0.1" 5500 :element-type 'character)))
    (unwind-protect
        (progn 
          (format t "==> Start~%")
          (loop do
           (usocket:wait-for-input socket)
           (load-state (usocket:socket-stream socket))
           (field-init)
           (format t "~v@{~A~:*~}~%" *size* "-") 
           (print-field)
           (clear-field))
      (progn 
       (format t "==> Closing socket~%")
       (usocket:socket-close socket))))))
