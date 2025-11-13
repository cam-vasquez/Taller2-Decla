;; TALLER 2 – FERNANDA CAMILA VASQUEZ MELENDEZ 00065221

;; --------------------------------------------------------------
;; EJERCICIO 1 – Contar elementos positivos en una lista
;; Objetivo: Usar filter y length para determinar cuántos elementos positivos hay.

(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

;; Entrada
(contar-positivos '(3 -2 7 0 -5 9)) ; → 3

;; --------------------------------------------------------------

;; EJERCICIO 2 – Generar lista de cuadrados pares

(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

;; Entrada
(cuadrados-pares '(1 2 3 4 5 6 7 8)) ; → '(4 16 36 64)

;; --------------------------------------------------------------

;; EJERCICIO 3 – Calcular el factorial de un número

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Entrada
(factorial 5) ; → 120


;; --------------------------------------------------------------
;; EJERCICIO 4 – Elevar cada número al cubo

(define (cubos lst)
  (map (lambda (x) (* x x x)) lst))

;; Entrada
(cubos '(2 3 4)) ; → '(8 27 64)

;; --------------------------------------------------------------
;; EJERCICIO 5 – Sumar todos los elementos impares

(define (sumar-impares lst)
  (foldl + 0 (filter odd? lst)))

;; Entrada
(sumar-impares '(1 2 3 4 5 6 7)) ; → 16

;; --------------------------------------------------------------
;; EJERCICIO 6 – Determinar si una lista contiene números negativos

(define (tiene-negativos? lst)
  (ormap (lambda (x) (< x 0)) lst))

;; Entrada
(tiene-negativos? '(5 9 -3 2)) ; → #t

;; --------------------------------------------------------------
;; EJERCICIO 7 – Calcular la suma acumulada de una lista

(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))

;; Entrada
(suma-acumulada '(1 2 3 4)) ; → '(1 3 6 10)


;; --------------------------------------------------------------
;; EJERCICIO 8 – Concatenar cadenas de texto en una lista

(define (concatenar-cadenas lst)
  (foldl string-append "" lst))

;; Entrada
(concatenar-cadenas '("Hola" " " "Mundo")) ; → "Hola Mundo"

;; --------------------------------------------------------------
;; EJERCICIO 9 – Generar lista con el doble de los números mayores que 5

(define (dobles-mayores-que5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

;; Entrada
(dobles-mayores-que5 '(3 6 8 2 10)) ; → '(12 16 20)

;; --------------------------------------------------------------
;; EJERCICIO 10 – Invertir el orden de una lista

(define (invertir-lista lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

;; Entrada

(invertir-lista '(1 2 3 4)) ; → '(4 3 2 1)

;; --------------------------------------------------------------
;; EJERCICIO 11 – Crear una función que reciba una función como parámetro
 
(define (aplicar-funcion f lst)
  (map f lst))

;; Entrada
(define (cuadrado x) (* x x))
(aplicar-funcion cuadrado '(1 2 3 4)) ; → '(1 4 9 16)


;; --------------------------------------------------------------
;; EJERCICIO 12 – Reto integrador
 
(define (promedio-mayores5 lst)
  (let* ([mayores (filter (lambda (x) (> x 5)) lst)]
         [suma (foldl + 0 mayores)]
         [cantidad (length mayores)])
    (/ suma cantidad)))

;; Entrada
(promedio-mayores5 '(3 8 10 4 9 2 7)) ; → 8.5
