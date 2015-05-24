(in-package :user)

;;; IMPROVED LIMITED DISCREPANCY SEARCH (LDS)

;;; Tempo limite de execução
(defconstant MAX-SECONDS 15)
	
(defun time-to-stop? (start-time n-seconds)
  "Verifica se ja ultrupassou o tempo de execução pre-definido."
  (<= (* n-seconds INTERNAL-TIME-UNITS-PER-SECOND) (- (get-start-time) start-time )))

(defun get-start-time ()
  "Devolve tempo atual."
  (get-internal-run-time))

;;;iterativo
;;;devolve ((estado1)(estado2)...(estado_objetivo))
(defun ILDS (state depth discrepancy)
  (let ((start-time (get-start-time)))
    (labels ((ILDS-probe (state path depth discrepancy start-time)
               (let ((lst-sucessors '())
                     (temp-path (list-copy path)))
                 (if (time-to-stop? start-time MAX-SECONDS)
                     (return-from ILDS nil))
                 ;;;(print "entering")
                 ;;;(print path)
                 ;;;(print state)
                 (if (objectivo? state)
                     (return-from ILDS path))
                 (setf lst-sucessors (sucessores state))
                 (if (null lst-sucessors) (return-from ILDS-probe NIL))
                 (if (> depth discrepancy)
                     (progn
                       (setf lst-sucessors (first lst-sucessors))
                       (setf temp-path (append temp-path (list lst-sucessors)))
                       (ILDS-probe lst-sucessors temp-path (- depth 1) discrepancy start-time))) 
                 (if (> discrepancy 0)
                     (progn
                       (setf lst-sucessors (rest lst-sucessors))
                       (loop for i from 0 to (- (length lst-sucessors) 1) do
                             (setf temp-path (append temp-path (list (nth i lst-sucessors))))
                             (ILDS-probe (nth i lst-sucessors) temp-path (- depth 1) (- discrepancy 1) start-time)
                             (setf temp-path (list-copy path))))))))       
    (loop for k from 0 to discrepancy do
         (ILDS-probe state (list state) depth k start-time)))))
         

      
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

(defun list-copy (lista)
  (let ((new-lst '()))
    (loop for i from 0 to (- (length lista)1) do
          (setf new-lst (append new-lst (list(nth i lista)))))
    new-lst))
  
            
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


  
    


  