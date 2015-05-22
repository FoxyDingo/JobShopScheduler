(in-package :user)

;; state representation : ( ( ( (JB1.task1) (JB1.task2) ) ( (JB2.task1) ) ) (MST1 MST2 MST3))
;;MSTx -> int to represent the maximum start time for a given machine
 ;;test state -> 
 ;; (list (list (list (make-job-shop-task :task.nr 0 :machine.nr 0 :start.time nil :duration 5 ) (make-job-shop-task :task.nr 1 :machine.nr 0 :start.time nil :duration 20))) (list 0))
;;test state 2 jobs -> 
;; (list (list (list (make-job-shop-task :task.nr 0 :machine.nr 0 :start.time nil :duration 5 )) (list (make-job-shop-task :task.nr 0 :machine.nr 0 :start.time nil :duration 20))) (list 0))
;; test state 2 jobs 2 tasks
;;(list (list (list (make-job-shop-task :task.nr 0 :machine.nr 0 :start.time nil :duration 5 ) (make-job-shop-task :task.nr 1 :machine.nr 1 :start.time nil :duration 10 )) (list (make-job-shop-task :task.nr 0 :machine.nr 1 :start.time nil :duration 20))) (list 0 0))

;;TODO try to make the search go task by task to see if its faster ::not optimal but is a good alternative method
;;TODO change list to array to see if its faster

(defun calendarizacao (estado-inicial procura)
  (let ((estado (problema-to-estado estado-inicial))
      (problema '())
      (resultado '())
      (estado-final '()))
    (setf problema (cria-problema estado (list 'gera-estados)
                    :objectivo? #'fnc-objetivo
                    :estado= #'equalp
                    :hash #'hash))
     (if (equalp procura 'profundidade) (setf resultado (procura problema procura :espaco-em-arvore? t)))
     (setf resultado (procura problema procura :espaco-em-arvore? t))))



(defun hash (estado)
   estado)

;;tested
(defun problema-to-estado (problema)
  (defvar *nrJobs* nil)
  ;;testing only
  (defvar *prob* '())
  (let ((lista-jobs '())
    (lista-tarefas '())
    (tmp '())
    (listamaquinas '())
    (lista '()))
    (setf *nrJobs* (job-shop-problem-n.jobs problema))
    (dotimes (nrJob *nrJobs*)
      (setf tmp (nth nrJob (job-shop-problem-jobs problema)))
      (setf lista-tarefas (job-shop-job-tasks tmp))
      (push lista-tarefas lista-jobs))
    (setf lista-jobs (reverse lista-jobs))
    (setf listamaquinas (make-list (job-shop-problem-n.machines problema)))
    (dotimes (n (length listamaquinas))
      (setf (nth n listamaquinas) 0))
    (setf lista (list lista-jobs listamaquinas))
    (setf *prob* lista)
    (return-from problema-to-estado lista)))



;;;Tested
(defun estado-tarefa (estado nrJob nrTask)
 (nth nrTask (nth nrJob (nth 0 estado))))

;;;Tested
(defun estado-job (estado nrJob)
  (nth nrJob (nth 0 estado)))

(defun estado-getStartTimeMachine (estado nrMachine)
  (nth nrMachine (estado-listaMaquinas estado)))

(defun estado-setStartTimeMachine (estado nrMachine time)
  (setf (nth nrMachine (estado-listaMaquinas estado)) time))

(defun estado-listaMaquinas (estado)
  (nth 1 estado))

(defun estado-setListaMaquinas (estado lista)
  (setf (nth 1 estado) (copy-list lista)))

(defun estado-setListaJobs (estado lista)
  (setf (nth 0 estado) lista))

(defun set-startTimeOfTask (estado nrJob nrTask startTime)
  (setf (job-shop-task-start.time (estado-tarefa estado nrJob nrTask)) startTime))


;;(defun estado-tarefa-machineNr (estado nrJob nrTask)
 ;; (job-shop-task-machine.nr (estado-tarefa estado nrJob nrTask)))

;;;Tested
(defun job-nrTasks (estado nrJob)
  (length (estado-job estado nrJob)))

;;;tested
(defun tarefa-hasStartTime? (estado nrJob nrTask)
  (let ((tarefa (estado-tarefa estado nrJob nrTask)))
    (if (equalp (job-shop-task-start.time tarefa) nil)
        (return-from tarefa-hasStartTime? nil)
      (return-from tarefa-hasStartTime? t))))  


;;;tested
(defun fnc-objetivo (estado)
  (dotimes (nrJob *nrJobs*)
    (dotimes (nrTask (job-nrTasks estado nrJob))
      (if (not (tarefa-hasStartTime? estado nrJob nrTask))
          (return-from fnc-objetivo nil))))
  (return-from fnc-objetivo t))
      

;;tNOT WORKING
;;TODO do this without using reverse!!
(defun copia-estado (estado)
  (let* ((resultado (make-list 2))
    (lista-jobs '())
    (lista-tarefas '())
    (rev '()))
  (estado-setListaMaquinas resultado (estado-listaMaquinas estado))
  (dotimes (nrJob *nrJobs*)
    (dotimes (nrTask (job-nrTasks estado nrJob))
      (push (copy-structure (estado-tarefa estado nrJob nrTask)) lista-tarefas)
      (setf rev (reverse lista-tarefas)))
    
    (push rev lista-jobs)
    (setf lista-tarefas '()))
  (setf lista-jobs (reverse lista-jobs))
  (estado-setListaJobs resultado lista-jobs)
  (return-from copia-estado resultado)))
  
;;;TODO falta verificar se o tempo da maquina é maior que o tempo da tarefa anterior
(defun gera-estados (estado)
  (let ((lista-estados '())
    (tmp-estado '())
    (startTime 0)
    (finishTime 0))
    (dotimes (nrJob *nrJobs*)
      (block checkJob
        (dotimes (nrTask (job-nrTasks estado nrJob))
          (block checkTask
            (if (tarefa-hasStartTime? estado nrJob nrTask)
              (progn
              ;;  (print 'here)
                (return-from checkTask t))
              (progn
                (setf tmp-estado (copia-estado estado))
              ;;  (print nrJob)
              ;;  (print (job-shop-task-job.nr (estado-tarefa tmp-estado nrJob nrTask)))
              ;;  (print nrTask)
              ;;  (print (job-shop-task-task.nr (estado-tarefa tmp-estado nrJob nrTask)))
                (setf startTime (estado-getStartTimeMachine tmp-estado (job-shop-task-machine.nr (estado-tarefa tmp-estado nrJob nrTask))))
                (if (> nrTask 0)
                  (progn 
                   ;; (print 'previous)
                   ;; (print nrJob)

                  ;;  (print (job-shop-task-job.nr (estado-tarefa tmp-estado nrJob (- nrTask 1))))
                  ;;  (print nrTask)
                  ;;  (print (job-shop-task-task.nr (estado-tarefa tmp-estado nrJob (- nrTask 1))))
                    (if (< startTime (job-shop-task-start.time (estado-tarefa tmp-estado nrJob (- nrTask 1) )))
                     (setf startTime (job-shop-task-start.time (estado-tarefa tmp-estado nrJob (- nrTask 1) ))))))
                (setf finishTime (+ startTime (job-shop-task-duration (estado-tarefa tmp-estado nrJob nrTask))))
                (estado-setStartTimeMachine tmp-estado (job-shop-task-machine.nr (estado-tarefa tmp-estado nrJob nrTask)) finishTime )
                (set-startTimeOfTask tmp-estado nrJob nrTask startTime)
                (push tmp-estado lista-estados)
                (setf startTime 0)
                (setf finishTime 0)
                (return-from checkJob t)))))))
   ;; (print 'generatedEverything)
    (return-from gera-estados  lista-estados)))











        
        
        
        
        
        
  

