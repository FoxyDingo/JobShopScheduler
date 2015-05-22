(in-package :user)

;;; Sondagem Iterativa

;;; Tempo limite de execução
(defconstant MAX-SECONDS 15)
	
(defun time-to-stop? (start-time n-seconds)
  "Verifica se ja ultrupassou o tempo de execução pre-definido."
  (<= (* n-seconds INTERNAL-TIME-UNITS-PER-SECOND) (- (get-start-time) start-time )))

(defun get-start-time ()
  "Devolve tempo atual."
  (get-internal-run-time))

(defun random-sucessor (lst-sucessors)
  "Recebe uma lista de sucessores e devolve um aleatoriamente."
  (let ((n-random (random (length lst-sucessors))))
    (nth n-random lst-sucessors)))


;;; devolve sequencia de estados no seguinte formato: ((estado1)(estado2)(estado3)...(estadon))
(defun random-probe (state)
  "Algoritmo sondagem iterativa. Procura estado que satisfaça, aleatoriamente, e devolve todos os estados até encontrar o objectivo"
  (let ((solution-state '())
        (solution '())
        (start-time (get-start-time)))
    (labels ((iter (state)
               (let ((lst-sucessors '()))
               (setf solution (append solution (list state)))
                 (if (objectivo? state)
                   solution
                   (progn (setf lst-sucessors (sucessores state))
                     (if (null lst-sucessors)
                         solution
                       (iter (random-sucessor lst-sucessors))))))))
    (loop (if (time-to-stop? start-time MAX-SECONDS)
              (progn
                (return-from random-probe nil))
          (progn
            (setf solution-state (iter state))
            (if (objectivo? (first (last solution-state)))
                (return-from random-probe solution-state))))))))

;;; POSSIVEL MELHORIA
;;; GUARDAR DAS SOLUCOES A MELHOR SOLUCAO, i.e, AQUELA COM MENOR COMPRIMENTO
;;; SE O TAMANHO DA LISTA FOR MENOR SUBSTITUI A SOLUCAO ATUAL, CASO CONTRARIO CONTINUA A PROCURA


;;; Testing 

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


    
    
    
  