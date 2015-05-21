(in-package :user)

;; state representation : ( ( ( (JB1.task1) (JB1.task2) ) ( (JB2.task1) ) ) *all-tasks*)
;;*all-tasks* -> int to represent which task number is being generated
;; test state -> (list (list (list (make-job-shop-task :task.nr 0 :start.time 1) (make-job-shop-task :task.nr 1 :start.time nil))) 1)




;;;Tested
(defun getTarefa (estado nrJob nrTask)
  ;; TODO fazer verificacao se existe essa tarefa na lista (ver list size)
  (nth nrTask (nth nrJob (nth 0 estado))))

;;;Tested
(defun getJob (estado nrJob)
  (nth nrJob (nth 0 estado)))

;;;Tested
(defun job-nrTasks (estado nrJob)
  (length (getJob estado nrJob)))

;;;tested
;;; IF TASK DOESNT EXIST RETURNS TRUE
(defun tarefa-hasStartTime? (estado nrJob nrTask)
  ;; TODO fazer verificacao se existe essa tarefa na lista (ver list size)
  (let ((tarefa (getTarefa estado nrJob nrTask)))
    (if (equalp (job-shop-task-start.time tarefa) nil)
        (return-from tarefa-hasStartTime? nil)
      (return-from tarefa-hasStartTime? t))))  

(defun get-currentTaskNumber (estado)
  (nth 1 estado))

(defun set-currentTaskNr (estado nr)
  (setf (nth 1 estado) nr))

;;;TODO has to check if it is the last job with that task number and without a starting time
(defun isLastJobTaskNr )

;;;tested
(defun fnc-objetivo (estado)
  (dotimes (nrJob *nrJobs*)
    (dotimes (nrTask (job-nrTasks nrJob))
      (if (not (tarefa-hasStartTime? estado nrJob nrTask))
          (return-from fnc-objetivo nil))))
  (return-from fnc-objetivo t))
      
;;(defun copia-estado (estado)
;;  (let ((resultado (make-list *nrTarefas*)))
;;    (dotimes (linha *nrTarefas*)
;;     (if (rainha? estado linha)
;;       (setf (nth linha resultado) (nth linha estado))))
;;    (return-from copia-estado resultado)))

;;TODO se gerar estado no ultimo job actualizar a variavel alltasks do estado
(defun gera-sucessores (estado)
  (let (
    (lista-estados '())
    (nrTask (get-currentTaskNumber estado))
    (newNrTask (get-currentTaskNumber estado))
      (dotimes (nrJob *nrJobs*)
        (block check
            (if (and (> (job-nrTasks estado nrJob) nrTask) (not (tarefa-hasStartTime? estado nrJob nrTask)))
              (progn 
                (if (isLastJobTaskNr (nrTask))
                  (setf newNrTask (+ nrTask 1)))
                (setf lista-estados (gera-estados estado nrTask newNrTask))
                (return-from gera-sucessores lista-estados))
              (return-from check t)))))))

;;TODO actualizar a variavel alltasks do estado
(defun gera-estados (estado nrTask newNrTask)
  (let (tmp-estado '())
    (lista-estados '())
    (higherStartTime nil)
    (dotimes (nrJob *nrJobs*)
      ;;;TODO check if copy-alist works
      (setf tmp-estado (copy-alist))
      (setf (set-currentTaskNr estado newNrTask))
      (setf higherStartTime (get-higherStartTime estado nrTask))
      ;;starttimeoftask has to see if higherStarTIme is bigger than the start time of the preceding tasks for the job
      (set-startTimeOfTask(estado nrJob nrTask higherStartTime))
      (push tmp-estado lista-estados))
    (return-from gera-estados lista-estados)))
 
        
        
        
        
        
        
  

