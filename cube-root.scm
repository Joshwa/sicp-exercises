; Structure and Interpretations of Computer Programs
; Exercise 1.8
; Create a program for determining cube roots via Newton's method

; function for determining the cube of x
(define (cube x)
  (* x x x))

; the iterator- if a guess is close enough to my threshold defined in 'good-enough?',
; the program exits and displays this guess
; if it doesn't, the iterator calls itself but with a modified guess passed through 'improve'
; ie continually iterates the 'guess' variable with the formula in 'improve'
(define (cube-root-iterator guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iterator (improve guess x) x)))

; 'improve' is just a procedure written to calculate Newton's method for approximating cube roots
(define (improve guess x)
  ( / 
    ( + ( / x (square guess))(* 2 guess))
    3))

; test to determine how close the guess is. threshold allows 0.01 of variability maximum
; takes the absolute value of the guess cubed , subtracting the cube x.
; if the difference is less than 0.01, check passes
; takes absolute value as to allow the guess to be 0.01 larger
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.01))

; main funcion call. 1.0 is the starting point of our guess
(define (cube-root x)
  (cube-root-iterator 1.0 x))
