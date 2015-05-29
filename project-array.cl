(in-package :user)

;;TODO LIST
;; fix ILDS TO GIVE BEST STATE
;; mudar calendarizacao para funcionar com os argumentos do profesor 

;;;;;;;;;;;;;;;;;;;;;;:;;;
;;; FUNCOES AUXILIARES ;;;
;;;;;;;;;;;;;;;;;;;;;;:;;;

(defvar *nrJobs* nil)
(defvar *job-list* (make-hash-table))
(defvar *prob-array* nil)
(defvar *nrMaxTasks* 0)
(defvar *max-tasks-per-job* 0)

(defvar *corte* 0)


;;; Tempo limite de execução
(defconstant MAX-SECONDS 60)
  
(defun time-to-stop? (start-time n-seconds)
  "Verifica se ja ultrupassou o tempo de execucao pre-definido."
  (<= (* n-seconds INTERNAL-TIME-UNITS-PER-SECOND) (- (get-start-time) start-time )))

(defun get-start-time ()
  "Devolve tempo atual."
  (get-internal-run-time))


(defun list-copy (lista)
  (let ((new-lst '()))
    (loop for i from 0 to (- (length lista)1) do
          (setf new-lst (append new-lst (list(nth i lista)))))
    new-lst))

(defun problema-to-estado (problema)
  (let ((array-jobs (make-array (job-shop-problem-n.jobs problema)))
    (lista-jobs '())
    (lista-tarefas nil)
    (job nil)
    (listamaquinas '())
    (lista '())
    (tasks-per-job 0)
    (max-tasks-per-job 0))
    (setf *nrJobs* (job-shop-problem-n.jobs problema))
    (dotimes (nrJob *nrJobs*)
      (setf (gethash nrJob *job-list*) (make-hash-table))
      (setf *corte* 0)
      (setf tasks-per-job 0)
      (setf job (nth nrJob (job-shop-problem-jobs problema)))
      (setf lista-tarefas (job-shop-job-tasks job))
      (push lista-tarefas lista-jobs)
      (setf (aref array-jobs nrJob) (make-array (length lista-tarefas)))
      (dotimes (nrTask (length lista-tarefas))
        (setf tasks-per-job (+ tasks-per-job 1))
        (setf *nrMaxTasks* (+ *nrMaxTasks* 1))
	(setf (gethash nrTask (gethash nrJob *job-list*)) (nth nrTask lista-tarefas))
        (setf (aref (aref array-jobs nrJob) nrTask) (job-shop-task-start.time (nth nrTask lista-tarefas))))
      (if (> tasks-per-job max-tasks-per-job)
        (setf max-tasks-per-job tasks-per-job)))
    (setf *max-tasks-per-job* max-tasks-per-job)
    (setf listamaquinas (make-list (job-shop-problem-n.machines problema)))
    (dotimes (n (length listamaquinas))
      (setf (nth n listamaquinas) 0))
    (setf lista (list array-jobs listamaquinas 0))
    (setf *prob-array* lista)
    lista))

(defun estado-tarefa (nrJob nrTask)
 (gethash nrTask (gethash nrJob *job-list*)))

(defun estado-getStartTimeMachine (estado nrMachine)
  (nth nrMachine (estado-listaMaquinas estado)))

(defun estado-setStartTimeMachine (estado nrMachine time)
  (setf (nth nrMachine (estado-listaMaquinas estado)) time))

(defun estado-listaMaquinas (estado)
  (nth 1 estado))

(defun estado-setListaMaquinas (estado lista)
  (setf (nth 1 estado) (copy-list lista)))

(defun estado-setArrayJobs (estado array)
  (setf (nth 0 estado) array))

(defun estado-getArrayJobs (estado)
  (nth 0 estado))

(defun set-startTimeOfTask (estado nrJob nrTask startTime)
  (setf (aref (aref (estado-getArrayJobs estado) nrJob) nrTask) startTime))

(defun get-startTimeOfTask (estado nrJob nrTask)
  (aref (aref (estado-getArrayJobs estado) nrJob) nrTask))

(defun get-finishTimeOfTask (estado nrJob nrTask)
  (let ((time 0))
    (setf time (aref (aref (estado-getArrayJobs estado) nrJob) nrTask))
    (setf time (+ time (job-shop-task-duration (estado-tarefa nrJob nrTask))))))

(defun estado-nrTarefas (estado)
  (nth 2 estado))

(defun estado-incNrTarefas (estado)
  (let ((nr (nth 2 estado)))
    (setf (nth 2 estado) (+ nr 1 ))))

(defun estado-setNrTarefas (estado nr)
  (setf (nth 2 estado) nr ))

(defun job-nrTasks (estado nrJob)
  (length (aref (estado-getArrayJobs estado) nrJob)))

(defun tarefa-hasStartTime? (estado nrJob nrTask)
  (not (equalp (aref (aref (nth 0 estado) nrJob) nrTask) nil)))

;;;;;;;;;;;;;;;;;;;;;;:;;;
;;; FUNCOES PROCURA    ;;;
;;;;;;;;;;;;;;;;;;;;;;:;;;

      
(defun fnc-objetivo (estado)
  (dotimes (nrJob *nrJobs*)
    (dotimes (nrTask (job-nrTasks estado nrJob))
      (if (not (tarefa-hasStartTime? estado nrJob nrTask))
          (return-from fnc-objetivo nil))))
  t)


(defun hash (estado)
   estado)

(defun estado-igual (estado1 estado2)
  (equalp (nth 0 estado1) (nth 0 estado2)))

(defun copia-estado (estado)
  (let ((resultado (make-list 3))
    (array-jobs (make-array *nrJobs*))
    (array-tasks nil))
    (dotimes (nrJob *nrJobs*)
      (setf array-tasks (make-array (job-nrTasks estado nrJob)))
      (dotimes (nrTask (job-nrTasks estado nrJob))
        (setf (aref array-tasks nrTask) (get-startTimeOfTask estado nrJob nrTask)))
      (setf (aref array-jobs nrJob) array-tasks))
    (estado-setListaMaquinas resultado (estado-listaMaquinas estado))
    (estado-setNrTarefas resultado (estado-nrTarefas estado))
    (estado-setArrayJobs resultado array-jobs)
    resultado))

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
                (return-from checkTask t))
              (progn
                (setf tmp-estado (copia-estado estado))
                (setf startTime (estado-getStartTimeMachine tmp-estado (job-shop-task-machine.nr (estado-tarefa nrJob nrTask))))
                (if (> nrTask 0)
                  (progn 
                    (if (< startTime (get-finishTimeOfTask tmp-estado nrJob (- nrTask 1) ))
                     (setf startTime (get-finishTimeOfTask tmp-estado nrJob (- nrTask 1) )))))
                (setf finishTime (+ startTime (job-shop-task-duration (estado-tarefa nrJob nrTask))))
                (estado-setStartTimeMachine tmp-estado (job-shop-task-machine.nr (estado-tarefa nrJob nrTask)) finishTime )
                (set-startTimeOfTask tmp-estado nrJob nrTask startTime)
                (estado-incNrTarefas tmp-estado)
                (setf lista-estados (nconc (list tmp-estado) lista-estados))
                (setf startTime 0)
                (setf finishTime 0)
                (return-from checkJob t)))))))
       lista-estados))

(defun gera-estados-corte (estado)
  (let ((lista-estados '())
    (tmp-estado '())
    (startTime 0)
    (finishTime 0)
    (tmpCorte)
    (nrCorteEstado (estado-nrTarefas estado)))
  (if (< (abs (- nrCorteEstado *corte*)) 2)
    (dotimes (nrJob *nrJobs*)
      (block checkJob
        (dotimes (nrTask (job-nrTasks estado nrJob))
          (block checkTask
            (if (tarefa-hasStartTime? estado nrJob nrTask)
              (progn
                (return-from checkTask t))
              (progn
                (setf tmp-estado (copia-estado estado))
                (setf startTime (estado-getStartTimeMachine tmp-estado (job-shop-task-machine.nr (estado-tarefa nrJob nrTask))))
                (if (> nrTask 0)
                  (progn 
                    (if (< startTime (get-finishTimeOfTask tmp-estado nrJob (- nrTask 1) ))
                     (setf startTime (get-finishTimeOfTask tmp-estado nrJob (- nrTask 1) )))))
                (setf finishTime (+ startTime (job-shop-task-duration (estado-tarefa nrJob nrTask))))
                (estado-setStartTimeMachine tmp-estado (job-shop-task-machine.nr (estado-tarefa nrJob nrTask)) finishTime )
                (set-startTimeOfTask tmp-estado nrJob nrTask startTime)
                (setf tmpCorte (estado-incNrTarefas tmp-estado))
                (setf lista-estados (nconc (list tmp-estado) lista-estados))
                (setf startTime 0)
                (setf finishTime 0)
                (if (> tmpCorte *corte*)
                  (setf *corte* tmpCorte))
                (return-from checkJob t))))))))
     lista-estados))

;;;;;;;;;;;;;;;;;;;;
;;;; HEURISTICAS ;;;
;;;;;;;;;;;;;;;;;;;;


(defun h1-maiorTempo (estado)
  (let ((maiorTempo 0)
    (machineTime 0))
  (dotimes (machineNr (length (estado-listaMaquinas estado)))
    (setf machineTime (estado-getStartTimeMachine estado machineNr))
    (if (> machineTime maiorTempo)
      (setf maiorTempo machineTime)))
  maiorTempo))

;;Best heuristic
(defun h2-tempoEtarefas (estado)
  (let ((tempo (h1-maiorTempo estado))
        (maiorTempo 0))
    (setf maiorTempo (- tempo  (* (estado-nrTarefas estado) 2)))
    (if (< maiorTempo 0)
      (setf maiorTempo 0))
  maiorTempo))
  
;;better than h1, worse than h2
(defun h3-tempoDescontos (estado)
  (let ((tempo (h1-maiorTempo estado))
        (maiorTempo 0)
        (maxTasks (+ *nrMaxTasks* 1))
        (nr 0))
    (setf nr (- maxTasks (estado-nrTarefas estado)))
    (setf nr (/ nr maxTasks))
    (setf maiorTempo (* tempo  nr))))

;;worse than h2, better than the rest
(defun h4-menorTempoUltimaTarefa (estado)
  (let (( menorTempo most-positive-fixnum)
    (tmp most-positive-fixnum))
    (dotimes (nrJob *nrJobs*)
      (block checkJob
        (dotimes (nrTask (job-nrTasks estado nrJob))
          (block checkTask
            (if (tarefa-hasStartTime? estado nrJob nrTask)
              (progn
                (setf tmp (get-startTimeOfTask estado nrJob nrTask)))
              (progn
                (return-from checkJob t)))))))
      (if (< tmp menorTempo)
        (setf menorTempo tmp))
      menorTempo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SONDAGEM ITERATIVA ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-sucessor (lst-sucessors)
  "Recebe uma lista de sucessores e devolve um aleatoriamente."
  (let ((n-random (random (length lst-sucessors))))
    (nth n-random lst-sucessors)))

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
                (if (or (null best-solution) (< (h1-maiorTempo (first (last solution-state))) (h1-maiorTempo (first (last best-solution)))))
                    (setf best-solution solution-state))))))))


;;;;;;;;;;;;;           
;;;; ILDS ;;;
;;;;;;;;;;;;;


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

(defun order-sucessors (sucessors heuristica)
  "Ordena uma lista usando a heuristica."
  (setf sucessors (sort sucessors heuristica)))

(defun order-sucessors-h1 (estado1 estado2)
  (< (abs (h1-maiorTempo estado1)) (abs (h1-maiorTempo estado2))))

(defun order-sucessors-h2 (estado1 estado2)
  (< (abs (h2-tempoEtarefas estado1)) (abs (h2-tempoEtarefas estado2))))

(defun ILDS (state depth discrepancy sucessores objectivo? heuristic)
  "Algoritmo ILDS. Percorre a arvore de estados tendo em conta o numero de descrepancias."
  (let ((start-time (get-start-time)))
    (labels ((ILDS-probe (state path depth discrepancy start-time)
               (let ((lst-sucessors '())
                     (temp-path (list-copy path)))
                 (if (time-to-stop? start-time MAX-SECONDS)
                     (return-from ILDS nil))
                 (if (funcall objectivo? state)
                     (return-from ILDS path))
                 (setf lst-sucessors (order-sucessors (funcall sucessores state) heuristic))
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

(defun ILDS-optimized (state depth discrepancy sucessores objectivo? heuristic)
  "Algoritmo ILDS. Guarda o melhor resultado e devolve apos x MAX-SECONDS"
  (let ((start-time (get-start-time))
        (solution '())
        (best-solution '()))
    (labels ((ILDS-probe (state path depth discrepancy start-time)
               (let ((lst-sucessors '())
                     (temp-path (list-copy path)))
                 (if (time-to-stop? start-time MAX-SECONDS)
                     (return-from ILDS-probe nil))
                 (if (funcall objectivo? state)
                     (return-from ILDS-probe path))
                 (setf lst-sucessors (order-sucessors (funcall sucessores state) heuristic))
                 (if (null lst-sucessors) (return-from ILDS-probe NIL))
                 (if (> depth discrepancy)
                     (progn
                       (setf temp-path (append temp-path (list (first lst-sucessors))))
                       (ILDS-probe (first lst-sucessors) temp-path (- depth 1) discrepancy start-time))) 
                 (if (> discrepancy 0)
                     (progn
                       (setf lst-sucessors (rest lst-sucessors))
                       (loop for i from 0 to (- (length lst-sucessors) 1) do
                             (setf temp-path (append temp-path (list (nth i lst-sucessors))))
                             (ILDS-probe (nth i lst-sucessors) temp-path (- depth 1) (- discrepancy 1) start-time)
                             (setf temp-path (list-copy path))))))))
      (loop for k from 0 to discrepancy do
            (setf solution (ILDS-probe state (list state) depth k start-time))
            (if (or (null best-solution) (< (h1-maiorTempo (first (last solution))) (h1-maiorTempo (first (last best-solution)))))
                (setf best-solution solution))
            (if (time-to-stop? start-time MAX-SECONDS)
                best-solution)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JOB SHOP SCHEDULER ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calendarizacao (estado-inicial procura)
  (let ((estado (problema-to-estado estado-inicial))
        (resultado '()))
    (if (equal procura "profundidade")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash) procura :espaco-em-arvore? t) ))
    (if (equal procura "sondagem.iterativa")
        (setf resultado (random-probe estado #'gera-estados #'fnc-objetivo)))
    (if (equal procura "sondagem.iterativa.optimizada")
        (setf resultado (random-probe-optimized estado #'gera-estados #'fnc-objetivo)))
    (if (equal procura "ILDS")
        (setf resultado (ILDS estado (depth estado #'gera-estados) (depth estado #'gera-estados) #'gera-estados #'fnc-objetivo #'order-sucessors-h1)))
    (if (equal procura "ILDS-optimizado")
        (setf resultado (ILDS-optimized estado (depth estado #'gera-estados) (depth estado #'gera-estados) #'gera-estados #'fnc-objetivo #'order-sucessors-h1)))
    (if (equal procura "a*")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h1-maiorTempo) procura :espaco-em-arvore? t) ))
    (if (equal procura "ida*")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h1-maiorTempo) procura :espaco-em-arvore? t) ))
     (if (equal procura "a*h2")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h2-tempoEtarefas) "a*" :espaco-em-arvore? t) ))
    (if (equal procura "ida*h2")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h2-tempoEtarefas) "ida*" :espaco-em-arvore? t) ))
    (if (equal procura "abordagem.alternativa")
        (setf resultado (abordagem-alternativa estado)))
    (if (equal procura "a*h3")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h3-tempoDescontos) "a*" :espaco-em-arvore? t) ))
    (if (equal procura "ida*h3")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h3-tempoDescontos) "ida*" :espaco-em-arvore? t) ))
        ;(setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'equalp :hash #'hash ) procura :espaco-em-arvore? t) ))
    (if (equal procura "a*h4")
        (setf resultado (procura (cria-problema estado (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h4-menorTempoUltimaTarefa) "a*" :espaco-em-arvore? t) ))
    (if (equal procura "a*h2-corte")
        (setf resultado (procura (cria-problema estado (list 'gera-estados-corte) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h2-tempoEtarefas) "a*" :espaco-em-arvore? t) ))
    (if (equal procura "a*h3-corte")
        (setf resultado (procura (cria-problema estado (list 'gera-estados-corte) :objectivo? #'fnc-objetivo :estado= #'estado-igual :hash #'hash :heuristica #'h3-tempoDescontos) "a*" :espaco-em-arvore? t) ))
    (job-list-to-final (actualiza-job-list (resultado-procura-to-estado resultado)))))

(defun actualiza-job-list (estado)
  (let ((jobs (estado-getArrayJobs estado)))
    (dotimes (nrJob *nrJobs*)
      (dotimes (nrTask (job-nrTasks estado nrJob))
        (setf (job-shop-task-start.time (gethash nrTask (gethash nrJob *job-list*))) (get-startTimeOfTask estado nrJob nrTask))))
    estado))

(defun job-list-to-final (estado)
  (let ((lista '()))
    (dotimes (nrJob *nrJobs*)
      (dotimes (nrTask (job-nrTasks estado nrJob))
        (push (gethash nrTask (gethash nrJob *job-list*)) lista)))
    (reverse lista)))
 



;;TODO see if these numbers work
(defun nrTasksIteration (jobs)
  (if (< jobs 4)
    (return-from nrTasksIteration 4))
  (if (< jobs 8)
    (return-from nrTasksIteration 3)
    (return-from nrTasksIteration 2)))

;;passa a informaçao do estado alternativo para o estado)
(defun actualiza-estado-com-alternativo (estado estadoAlt iterationNr nrTasksInIteration)
  (let ((itNr (* iterationNr nrTasksInIteration)))
    (dotimes (nrJob *nrJobs*)
      (dotimes (nrTask (job-nrTasks estadoAlt nrJob))
        (set-startTimeOfTask estado nrJob nrTask (get-startTimeOfTask estadoAlt nrJob nrTask))))
  (estado-setNrTarefas estado (estado-nrTarefas estadoAlt))
  (estado-setListaMaquinas estado (estado-listaMaquinas estadoAlt))))

(defun resultado-procura-to-estado (resProcura)
  (car (last (first resProcura))))

(defun abordagem-alternativa (estado) 
  (let* ((nrTaskMax *max-tasks-per-job*)
    (nrTasksInIteration (nrTasksIteration *nrJobs*))
    (estadoAlt nil)
    (resultadoP '())
    (resultado '())
    (nrCicles 0))
    (if (zerop (rem nrTaskMax nrTasksInIteration))
      (setf nrCicles (floor (/ nrTaskMax nrTasksInIteration)))
      (setf nrCicles (+ (floor (/ nrTaskMax nrTasksInIteration)) 1)))
    (dotimes (iterationNr nrCicles)
      (setf estadoAlt (cria-estado-alternativo estado iterationNr nrTasksInIteration))
      (setf resultadoP (procura (cria-problema estadoAlt (list 'gera-estados) :objectivo? #'fnc-objetivo :estado= #'equalp :hash #'hash :heuristica #'h2-tempoEtarefas) "a*" :espaco-em-arvore? t) )
      (setf resultado (resultado-procura-to-estado resultadoP))
      (actualiza-estado-com-alternativo estado resultado iterationNr nrTasksInIteration))
    (return-from abordagem-alternativa (list (list nil nil estado) nil nil nil))))  



(defun cria-estado-alternativo (estado iterationNr nrTasksInIteration)
  (let ((estadoAlt (make-list 3))
      (array-jobs (make-array *nrJobs*))
      (nrTask 0)
      (nrTasksLeft 0)
      (jobNrTasks nil)
      (itNr (* (+ iterationNr 1) nrTasksInIteration)))
    (dotimes (nrJob *nrJobs*)
      (block checkJob
        (setf jobNrTasks (job-nrTasks estado nrJob))
        (if (< jobNrTasks (* iterationNr nrTasksInIteration))
          (return-from checkJob t))
        (if (< jobNrTasks (* (+ iterationNr 1) nrTasksInIteration))
          (setf nrTasksLeft jobNrTasks )
          (setf nrTasksLeft itNr))
        (setf (aref array-jobs nrJob) (make-array nrTasksLeft))
        (dotimes (nr nrTasksLeft)
          (setf nrTask nr)
          (setf (aref (aref array-jobs nrJob) nr) (get-startTimeOfTask estado nrJob nrTask)))))
    (estado-setNrTarefas estadoAlt (estado-nrTarefas estado))
    (estado-setListaMaquinas estadoAlt (estado-listaMaquinas estado))
    (estado-setArrayJobs estadoAlt array-jobs)
    (return-from cria-estado-alternativo estadoAlt)))


;;NOTAS h2 parece explorar menos estados que h1
;;; NOTA escrever no relatorio que tanto em h1 e h2 apenas interessa o tempo da maquina com maior tempo pois o objectivo é ter a calendarizacao com menor tempo de todas as tarefas e não com menor tempo EM CADA tarefa
;; TODO fazer testes para os varios valores de iterationNr

