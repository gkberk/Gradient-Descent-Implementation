; sabri gokberk yilmaz
; 2017400144
; compiling: yes
; complete: yes

#lang racket
(provide (all-defined-out))

;; given
(struct num (value grad)
	#:property prop:custom-write
	(lambda (num port write?)
		(fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
   		(num-value num) (num-grad num))))
;; given
; (define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
;; given
; (define mse (lambda (x y) (mul (sub x y) (sub x y))))

; 3.1
; this function gets value of a single element
(define (get-val element)
  (if (list? element)
  (num-value (eval element))
  (num-value element)
  ))
(define (get-value elements)
  (if (list? elements)
  (map get-val elements)
  (num-value elements)
  ))

; 3.2
; this function gets 
(define (get-gr element)
  (if (list? element)
  (num-grad (eval element))
  (num-grad element)
  ))
(define (get-grad elements)
  (if (list? elements)
  (map get-gr elements)
  (num-grad elements)
  ))

; 4.1
; this function puts its arguments into a list. Its input is args, however many inputs can be given. 
(define lister (lambda args (foldr cons '() args)))

; helper of add function. Applies + to vals and grads 
(define (addx-helper vals grads)
  (list 'num (apply + vals) (apply + grads)))

; add function
(define add(lambda args (let* ([x (lister args)]
                                [vals (get-value (car x))]
                                [grads (get-grad (car x))]
                                [additions (addx-helper vals grads)]) (eval additions))
          ))

; 4.3
;sub helper, applies - to vals and grads
(define (subx-helper vals grads)
  (list 'num (apply - vals) (apply - grads)))
(define sub(lambda args (let* ([x (lister args)]
                               [vals (get-value (car x))]
                               [grads (get-grad (car x))]
                               [subs (subx-helper vals grads)]) (eval subs))
                          ))

; 4.2
; this function multiplies grad of the first num with the values of the rest nums.
(define (mul-helper1 nums)
  (eval (cons '* (cons (get-grad(eval (car nums))) (get-value(cdr nums)))))
  ;(get-value(cdr nums))
  ;(get-grad (eval (car nums)))
  )

;this function puts indexth element to 0th index
(define (adjust-location list index)
  (cons (list-ref list index) (remv (list-ref list index) list)))

; this functions works like a loop. invokes adjust location function for all indexes
(define (loop-func list size outputs)
  (if (= size 1)
      (cons (mul-helper1 (adjust-location list 0)) '())
      (cons (mul-helper1 (adjust-location list (- size 1))) (loop-func list (- size 1) outputs))
      )
  )
(define (mul-helper2 vals)
  (apply * vals))

; mul function
(define mul(lambda args (let* ([x (lister args)]
                               [carx (car x)]
                               [gradpart (eval (cons '+ (loop-func (car x) (length (car x)) '())))]
                               [vals (get-value (car x))]
                               [valpart (apply * vals)]) (eval (list 'num valpart gradpart)))
             ))

; 4.4
;these functions are given in the description
(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
(define mse (lambda (x y) (mul (sub x y) (sub x y))))

; 5.1
; it mathces a single name with its value
(define (hash-helper name value var)
  (if (eqv? name var)
      (list name (eval(list 'num value 1.0)))
      (list name (eval(list 'num value 0.0)))
   )
  )
; this functions repats a value as much as size
(define (repeated-list var size)
  (if (= size 1)
      (cons var '())
      (cons var (repeated-list var (- size 1)))
      ))
; create-hash function
(define (create-hash names values var)
  (let*([varlist (repeated-list var (length values))]
        [intermed (map hash-helper names values varlist)]
        ) (apply hash(apply append intermed))))

; 5.2 parse function
(define (parse hash expr)
  (cond
    [(empty? expr) '()]
    [(list? expr) (cons (parse hash (car expr)) (parse hash (cdr expr)))]
    [(eq? expr '+) 'add]
    [(eq? expr '*) 'mul]
    [(eq? expr '-) 'sub]
    [(eq? expr 'mse) 'mse]
    [(eq? expr 'relu) 'relu]
    [(number? expr) (num expr 0.0)]
    [else (let ([myhash hash]) (hash-ref myhash expr))]
    ))

; 5.3 grad function

(define (grad names values var expr)
  (let* ([hash-tab (create-hash names values var)]
         [res (parse hash-tab expr)]) (get-grad (eval res))))


; 5.4
; partial-graddd is a helper for partial-grad
(define (partial-graddd names values vars expr ind)
  (if (= ind (length names))
      '()
      (cons
       (if (member (list-ref names ind) vars)
           (grad names values (list-ref names ind) expr)
           0.0
        )
       (partial-graddd names values vars expr (+ ind 1)))
   )
  )
; partial-grad
(define (partial-grad names values vars expr)
  (partial-graddd names values vars expr 0))

(define (aloop n)
   (if (= n 0)
      (cons 0 '())
      (cons n (aloop (- n 1)))
   ))

; 5.5
; gradient-descent
(define (gradient-descent names values vars lr expr)
  (map - values
       (map * (repeated-list lr (length names)) (partial-grad names values vars expr))
  ))

; 5.6
; optimize
(define (optimize names values vars lr k expr)
  (if (= k 1)
      (gradient-descent names values vars lr expr)
      (optimize names (gradient-descent names values vars lr expr) vars lr (- k 1) expr)
   )
  )