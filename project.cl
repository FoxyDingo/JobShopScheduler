(in-package :user)

;;; State representation : ( ( ( (JB1.task1) (JB1.task2) ) ( (JB2.task1) ) ) (MST1 MST2 MST3))
;;; MSTx -> int to represent the maximum start time for a given machine

;; TODO Correct algorithms output (only last state) from procura.lisp
;; TODO Alternative Search: try to make the search go task by task to see if its faster ::not optimal but is a good alternative method
;; TODO add order-sucessors to ILDS when generating sucessors
;; TODO Heuristics
;; TODO change list to array to see if its faster

;;;;;;;;;;;;;;;;;;;;;;:;;;
;;; FUNCOES AUXILIARES ;;;
;;;;;;;;;;;;;;;;;;;;;;:;;;

(defvar *nrJobs* nil)
(defvar *prob* '())

;;; Tempo limite de execução
(defconstant MAX-SECONDS 5)
	
(defun time-to-stop? (start-time n-seconds)
  "Verifica se ja ultrupassou o tempo de execução pre-definido."
  (<= (* n-seconds INTERNAL-TIME-UNITS-PER-SECOND) (- (get-start-time) start-time )))

(defun get-start-time ()
  "Devolve tempo atual."
  (get-internal-run-time))


(defun list-copy (lista)
  (let ((new-lst '()))
    (loop for i from 0 to (- (length lista)1) do
          (setf new-lst (append new-lst (list(nth i lista)))))
    new-lst))

;;tested
(defun problema-to-estado (problema)
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


(defun hash (estado)
   estado)

      
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
  
;;;TODO falta verificar se o tempo da maquina Ã© maior que o tempo da tarefa anterior
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

;;;;;;;;;;;;;;;;;;;;
;;;; HEURISTICAS ;;;
;;;;;;;;;;;;;;;;;;;;

;;;TODO
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SONDAGEM ITERATIVA ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-sucessor (lst-sucessors)
  "Recebe uma lista de sucessores e devolve um aleatoriamente."
  (let ((n-random (random (length lst-sucessors))))
    (nth n-random lst-sucessors)))


;;;devolve ((estado1)(estado2)...(estado_objetivo))
(defun random-probe (state sucessores objectivo?)
  "Algoritmo sondagem iterativa. Procura estado que satisfaça, aleatoriamente, e devolve todos os estados até encontrar o objectivo"
  (let ((solution-state '())
        (solution '())
        (start-time (get-start-time)))
    (labels ((iter (state)
               (let ((lst-sucessors '()))
                 (setf solution (append solution (list state)))
                 (if (funcall objectivo? state)
                     solution
                   (progn 
                     (setf lst-sucessors (funcall sucessores state))
                     (if (null lst-sucessors)
                         nil
                       (iter (random-sucessor lst-sucessors))))))))
      (loop 
        (if (time-to-stop? start-time MAX-SECONDS)
            (progn
              (return-from random-probe nil))
          (progn
            (setf solution-state (iter state))
            (if (funcall objectivo? (first (last solution-state)))
                (return-from random-probe solution-state))))))))

(defun random-probe-optimized (state sucessores objectivo?)
  "Algoritmo sondagem iterativa optimizada. Guarda o melhor estado,i.e menor comprimento, devolve estado passado MAX-SECONDS"
  (let ((solution-state '())
        (solution '())
        (best-solution '())
        (start-time (get-start-time)))
    (labels ((iter (state)
               (let ((lst-sucessors '()))
                 (setf solution (append solution (list state)))
                 (if (funcall objectivo? state)
                     solution
                   (progn 
                     (setf lst-sucessors (funcall sucessores state))
                     (if (null lst-sucessors)
                         nil
                       (iter (random-sucessor lst-sucessors))))))))
      (loop 
        (if (time-to-stop? start-time MAX-SECONDS)
              (return-from random-probe-optimized best-solution))
          (progn
            (setf solution-state (iter state))
            (if (funcall objectivo? (first (last solution-state)))
                (if (or (null best-solution) (< (length solution-state) (length best-solution)))
                    (setf best-solution solution-state))))))))


;;;;;;;;;;;;;           
;;;; ILDS ;;;
;;;;;;;;;;;;;

;;; MISSING -> FUNCAO ORDENA NÓS

(defun depth (state sucessores)
  "Devolve a profundidade maxima de um estado, i.e quanto sucessores tem."
  (let ((depth 0)
        (lst-sucessors '()))
    (loop (setf lst-sucessors (funcall sucessores state))
          (if (not(null lst-sucessors))
              (progn 
                (setf depth (incf depth))
                (setf state (first lst-sucessors)))
            (return-from depth depth)))))

;;; not used yet, heuristic not defined
(defun order-sucessors (sucessors heuristica)
  "Ordena uma lista usando a heuristica."
  (setf sucessors (sort sucessors heuristica)))

;;;iterativo
;;;devolve ((estado1)(estado2)...(estado_objetivo))
(defun ILDS (state depth discrepancy sucessores objectivo?)
  "Algoritmo ILDS. Percorre a arvore de estados tendo em conta o numero de descrepancias."
  (let ((start-time (get-start-time)))
    (labels ((ILDS-probe (state path depth discrepancy start-time)
               (let ((lst-sucessors '())
                     (temp-path (list-copy path)))
                 (if (time-to-stop? start-time MAX-SECONDS)
                     (return-from ILDS nil))
                 (if (funcall objectivo? state)
                     (return-from ILDS path))
                 (setf lst-sucessors (funcall sucessores state))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JOB SHOP SCHEDULER ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;devolver apenas ultimo estado
;;; add procuras certas
(defun calendarizacao (estado-inicial procura)
  (let ((estado (problema-to-estado estado-inicial))
        (resultado '()))
    (if (equal procura "profundidade")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'equalp :hash #'hash) procura)))
    (if (equal procura "sondagem.iterativa")
        (setf resultado (last (random-probe estado #'gera-estados #'fnc-objetivo))))
    (if (equal procura "sondagem.iterativa.optimizada")
        (setf resultado (last (random-probe-optimized estado #'gera-estados #'fnc-objetivo))))
    (if (equal procura "ILDS")
        (setf resultado (last (ILDS estado (depth estado #'gera-estados) (depth estado #'gera-estados) #'gera-estados #'fnc-objetivo))))
    resultado))





