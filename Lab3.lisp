;1 task

(defun swap-adjacent (list)

  (cond
    ((null list) nil)
    ((null (cdr list)) list)
    ((> (car list) (cadr list))       
      (cons (cadr list) (cons (car list) (swap-adjacent (cddr list)))))
    (t
     (cons (car list) (swap-adjacent (cdr list))))))


(defun bubble-sort-functional (list)
  (let ((swapped-list (swap-adjacent list)))
    (if (equal swapped-list list)         
      (bubble-sort-functional swapped-list))))



(defun run-bubble-sort-functional-tests ()
  (format t "Test 1: bubble-sort-functional ~%")
  (format t "~a~%" (equal (bubble-sort-functional '(4 2 5 1 3)) '(1 2 3 4 5)))
  (format t "Test 2: bubble-sort-functional ~%")
  (format t "~a~%" (equal (bubble-sort-functional '(1 2 3 4 5)) '(1 2 3 4 5)))
  (format t "Test 3: bubble-sort-functional (empty list) ~%")
  (format t "~a~%" (equal (bubble-sort-functional '()) '())))


(run-bubble-sort-functional-tests)


;2 task

(defun bubble-sort-imperative (list)
    (let ((copy (copy-list list)))  
       (do ((swapped t)
         (n (length copy)))
        ((not swapped))  
      (setf swapped nil)
      (dotimes (i (1- n))
        (when (> (nth i copy) (nth (1+ i) copy))  
          (rotatef (nth i copy) (nth (1+ i) copy)) 
          (setf swapped t))))
    copy)) 


(defun run-bubble-sort-imperative-tests ()
  (format t "Test 1: bubble-sort-imperative ~%")
  (format t "~a~%" (equal (bubble-sort-imperative '(4 2 5 1 3)) '(1 2 3 4 5)))
  (format t "Test 2: bubble-sort-imperative ~%")
  (format t "~a~%" (equal (bubble-sort-imperative '(1 2 3 4 5)) '(1 2 3 4 5)))
  (format t "Test 3: bubble-sort-imperative (empty list) ~%")
  (format t "~a~%" (equal (bubble-sort-imperative '()) '())))

(run-bubble-sort-imperative-tests)