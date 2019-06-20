#lang scheme

;Constructor
(define (constructor padre izq der)
  (list padre izq der))

; Definicion de constante vacio
(define vacio '())

; Selectores

(define (padre arbol)
  (car arbol))

(define (izq arbol)
  (car (cdr arbol)))

(define (der arbol)
  (car (cdr(cdr arbol))))

; arbol vacio?
(define (vacio? arbol)
  (null? arbol))

; el arbol posee ramas?
(define (hoja? arbol)
  (and (vacio? (izq arbol)) 
       (vacio? (der arbol))))

; Convertir arbol en lista
(define (arbol-a-lista arbol)
  ; si el arbol esta vacio no enlisto nada
  (if (vacio? arbol)
      '() 
  ; creo una lista y enlisto recursivamente la rama izquierd del arbol
  (append(arbol-a-lista (izq arbol))
         ; enlisto la raiz del arbol
         (list (padre arbol))
         ; enlisto recusivamente la rama derecha del arbol
         (arbol-a-lista (der arbol))))) 

;Buscar elemento en arbol 
(define (buscar x arbol)
  (cond
    ; si esta vacio el elemento no se encuentra en el arbol
    ((vacio? arbol) #f)
    ; si la raiz del arbol es igual al elemento lo encontro
    ((= x (padre arbol)) #t)
    ; si el elemento es mas chico que la raiz, buscar en la rama izquierda
    ((< x (padre arbol)) (buscar x (izq arbol)))
    ((> x (padre arbol)) (buscar x (der arbol)))
  ))

; Insertar elemento en arbol
(define (insertar x arbol)
  (cond
    ; si arbol esta vacio crear arbol con el elemento como raiz
    ((vacio? arbol) (constructor x vacio vacio))
    ; si el elemento es menor que la raiz del arbol
    ((< x (padre arbol))
     ; crear arbol con padre como raiz
     (constructor (padre arbol)
                  ; insertar elemento en rama izquierda del arbol
                  (insertar x (izq arbol) )
                  ; rama derecha del arbol
                  (der arbol)))
    ; si el elemento es mayor que la raiz del arbol
    ((> x (padre arbol))
     ; crear arbol con padre como raiz
     (constructor (padre arbol)
                  ; rama izquierda del arbol
                  (izq arbol)
                  ; insertar elemento en rama derecha del arbol
                  (insertar x (der arbol))
                  ))
  ; si el elemento ya se encuentra en el arbol devolver el arbol
    ((= x (padre arbol)) arbol)))

;Insertar lista en arbol
(define (insertar-lista lista arbol)
  ; si no hay que insertar devuelvo al arbol
  (if (vacio? lista) arbol
      ; inserto recursivamente los elementos de la lista
      ; llamo a insertar-lista con un elemento menos en la lista en cada recursividad
      ; e inserto ese elemento en el arbol
     (insertar-lista (cdr lista) (insertar (car lista) arbol ))))

; Converitr una lista en un arbol
(define (lista-a-arbol lista)
  ; insertar los elementos de la lista en un arbol vacio
  (insertar-lista lista vacio))