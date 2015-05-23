(in-package :user)

;;; IMPROVED LIMITED DISCREPANCY SEARCH (LDS)


;;; add time limit
(defun ILDS (state depth discrepancy)
  (let ((solution '()))
    (loop for k from 0 to discrepancy do
          (setf solution (ILDS-probe state depth k))
          (print solution)
          (if (not (null solution))
              (if (objectivo? solution)
                  (return-from ILDS solution))))))


(defun ILDS-probe (state depth discrepancy)
  (let ((solution '())
        (lst-sucessors '()))
    (if (objectivo? state)
        (progn (print "if1") (princ state)
        (return-from ILDS-probe state)))
    (setf lst-sucessors (sucessores state))
    (if (null lst-sucessors) nil)
    (if (> depth discrepancy)
        (progn
          (setf lst-sucessors (first lst-sucessors))
          (print "if3")
          (princ lst-sucessors)
          (setf solution (ILDS-probe lst-sucessors (- depth 1) discrepancy))
          (if (objectivo? solution)
              (return-from ILDS-probe solution))))
    (if (> discrepancy 0)
        (progn
          (setf lst-sucessors (rest lst-sucessors))
          (print "if4")
          (princ lst-sucessors)
          (loop for i from 0 to (- (length lst-sucessors)1) do
                (setf solution (ILDS-probe (nth i lst-sucessors) (- depth 1) (- discrepancy 1)))
                (if (objectivo? solution)
                    (return-from ILDS-probe solution)))))))
          
      
;;; should work with js sucessor function
(defun depth (state)
  (let ((depth 0)
        (lst-sucessors '()))
    (loop (setf lst-sucessors (sucessores state))
          (if (not(null lst-sucessors))
              (progn 
                (setf depth (incf depth))
                (setf state (first lst-sucessors)))
            (return-from depth depth)))))
 
  
            
;;; TESTING

(defvar *run* nil)

(defun sucessores (lista1)
  (let ((lista '()))
        (loop for i from 0 to (- (length lista1) 1) do
          (setf lista (append lista (list (list (nth i lista1) 1)))))
    lista))

(defun objectivo? (lista) 
  (if (equal lista '(1 1))
      T
    nil))

(defun estado= ()
  #'equalp)


  
    


  