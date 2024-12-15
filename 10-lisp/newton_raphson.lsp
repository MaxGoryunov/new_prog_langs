(defun remove-nth (n list)
  (remove-if (constantly t) list :start n :count 1))

(defun differentiate (expr var)
  (cond
    ((numberp expr) 0)
    ((eq expr var) 1)
    ((listp expr)
    (if (eq (car expr) '+)
        (mapcar (lambda (term) (differentiate term var)) (cdr expr))
        (if (eq (car expr) '*)
              (let ((terms (cdr expr)) (prod_sum '(+)))
                (format t "terms = ~a~%" terms)
                (loop for i from 0 below (length terms) by 1 do
                  (let* ((u (nth i terms))
                        (v (remove-nth i terms))
                        (curval (concatenate 'list '(*) (list (differentiate u var)) v )) )
                    (format t "curval = ~a~%" curval)
                    (setf prod_sum (append prod_sum (list curval)))
                    
                  )
                )
                prod_sum

              ) 
          expr)))
    (t expr)))

(defun newton-raphson (f df x0 tolerance max-iterations)
  (let ((x x0)
        (iteration 0))
    (loop
      (let ((fx (funcall f x))
            (dfx (funcall df x)))
        (if (< (abs fx) tolerance)
            (return x))

        (if (= dfx 0)
            (error "Derivative is zero; no solution found."))

        (setf x (- x (/ fx dfx)))
        (setf iteration (+ iteration 1))

        (when (>= iteration max-iterations)
          (error "Maximum iterations reached; no solution found."))))))

(format t "Derivative of f = ~a~%" (differentiate '(+ (* -5 x x x x x) (* 3 x) 2) 'x))

(defun f (x) (+ (* -5 x x x x x) (* 3 x) 2))
(defun df (x) (+ (* -25 x x x x) 3))

(let ((root (newton-raphson #'f #'df 0.5 1e-6 100)))
  (format t "Root found: ~a~%Value at root: ~a~%" root (f root)))
  




