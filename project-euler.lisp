; vim: set foldmethod=marker commentstring=;\ %s :

; strictperl -MList::Util=sum -e 'my @numbers = grep { ($_ % 3 == 0) or ($_ % 5 == 0) } 1 .. 999; print sum(@numbers);'
; 233168
(defun project-euler-1-1 (); {{{
  (let ((total 0))
    (dotimes (current-number 1000 total)
      (cond
        ((zerop (mod current-number 3)) (setf total (+ total current-number)))
        ((zerop (mod current-number 5)) (setf total (+ total current-number)))
      )
    )
  )
); }}}

(defun project-euler-1-2 (); {{{
  (project-euler-1-2-aux '0 '0)
); }}}
(defun project-euler-1-2-aux (current-number total); {{{
  (cond
    ((equal current-number 1000) total)
    ((or
        (zerop (mod current-number 3))
        (zerop (mod current-number 5))
      )
      (project-euler-1-2-aux (1+ current-number) (+ total current-number))
    )
    (t (project-euler-1-2-aux (1+ current-number) total))
  )
); }}}




; Create a closure that will return the next Fibonacci number each time it's
; called.  I guessed how to write a closure in LISP; I'd prefer to create an
; anonymous closure at the start of project-euler-2-1, but I haven't figured
; that out yet - I think it's several chapters away in my LISP book.
(let
  (
    (fib-2 0)
    (fib-1 1)
    (fib-count 2)
  )
  (defun fib-next (); {{{
    (let
      (
        (fib-current (+ fib-2 fib-1))
      )
      (setf fib-2 fib-1)
      (setf fib-1 fib-current)
      (setf fib-count (1+ fib-count))
      (print (list fib-count ": " fib-current))
      fib-current
    )
  ); }}}
)

; I think I could use optional parameters instead of defining an auxilary
; function - I'll try that next time.
(defun project-euler-2-1 (); {{{
  (project-euler-2-1-aux '0)
); }}}

(defun project-euler-2-1-aux (total); {{{
  (let
    (
      (fib-current (fib-next))
    )
    (cond
      ((> fib-current 4000000)
        total
      )
      ((evenp fib-current)
        (print (list "total: " (+ total fib-current)))
        (project-euler-2-1-aux (+ total fib-current))
      )
      (t
        (project-euler-2-1-aux total)
      )
    )
  )
); }}}

; The prime factors of 13195 are 5, 7, 13 and 29.
;
; What is the largest prime factor of the number 600851475143 ?

; get the factors of 600851475143
; test each one for primeness, starting with the largest

(defun primep (an-integer); {{{
  (dotimes (current-factor (1+ (ceiling (sqrt an-integer))) t)
    (cond
      ((<= current-factor 1)
        t
      )
      ((zerop (mod an-integer current-factor))
        (return nil)
      )
    )
  )
); }}}

(defun get-factors (an-integer); {{{
  (do*
    (
      (possible-factors (make-sequence-generator 2 (ceiling (sqrt an-integer))))
      (current-factor (funcall possible-factors))
      (factors nil)
    )
    ((null current-factor) factors)
    (when (zerop (mod an-integer current-factor))
      (setf factors (cons current-factor factors))
      (setf factors (cons (/ an-integer current-factor) factors))
    )
    (setf current-factor (funcall possible-factors))
  )
); }}}

(defun project-euler-3-1 (); {{{
  (setf magic-number 600851475143)
  (setf factors (get-factors magic-number))
  (setf sorted-factors (sort factors #'>))
  (setf highest-prime-factor
    (dolist (current-factor sorted-factors)
      (when (primep current-factor)
        (return current-factor)
      )
    )
  )
); }}}

(defun make-sequence-generator (start end); {{{
  (let
    (
      (last-number start)
    )
    (lambda ()
      (if (> last-number end)
        nil
        (let
          (
            (result last-number)
          )
          (setf last-number (1+ last-number))
          result
        )
      )
    )
  )
); }}}


; A palindromic number reads the same both ways. The largest palindrome made
; from the product of two 2-digit numbers is 9009 = 91 × 99.
;
; Find the largest palindrome made from the product of two 3-digit numbers.
(defun project-euler-4-1 (); {{{
  (let
    (
      (palindrome 0)
      (numbers nil)
    )
    (do
      ((outer-number 999))
      ((or (< outer-number 100)
            ; Stop when it's not possible to produce a product larger than the
            ; current palindrome.
           (< outer-number (/ palindrome outer-number))
      ))

      (do
        ((inner-number outer-number))
        ((< inner-number 100))

        (let*
          (
            (product (* outer-number inner-number))
            (product-string (write-to-string product))
            (reversed-product-string (reverse product-string))
          )
          (when (and
                  (> product palindrome)
                  (equal product-string reversed-product-string)
                )
            (setf palindrome product)
            (setf numbers (list outer-number inner-number ))
          )
        )
        (setf inner-number (1- inner-number))
      )

      (setf outer-number (1- outer-number))
    )
    (setf result (cons palindrome numbers))
  )
); }}}

; 2520 is the smallest number that can be divided by each of the numbers from 1
; to 10 without any remainder.
;
; What is the smallest number that is evenly divisible by all of the numbers
; from 1 to 20?

; Find the prime factors of each number between 1 and 20, including the power,
; so 9 = 3**2.  The smallest number is the product of the highest prime factors.

(defun project-euler-5-1 (); {{{
  (setf highest-number 20)
  ; I don't want to be constantly adding or subtracting 1 to indices.
  (setf array-size (1+ highest-number))
  ; This breaks the generalisation, but it could easily be replaced if
  ; necessary.
  (setf primes '(2 3 5 7 11 13 17 19))
  (setf factors-array (make-zeroed-array array-size))

  (do
    (
      (current-number 2)
    )
    (
      (> current-number highest-number)
    )

    (let
      (
        (remainder current-number)
        (current-factors (make-zeroed-array array-size))
      )
      (dolist (current-prime primes)
        (loop
          (when (not (zerop (mod remainder current-prime)))
            (return)
          )
          (setf remainder (/ remainder current-prime))
          (setf (aref current-factors current-prime)
                  (1+ (aref current-factors current-prime)))
        )
      )
      (do
        (
          (i 2)
        )
        (
          (> i highest-number)
        )
        (when (>
                (aref current-factors i)
                (aref factors-array i)
              )
          (setf (aref factors-array i)
                  (aref current-factors i))
        )
        (setf i (1+ i))
      )
    )
    (setf current-number (1+ current-number))
  )
  (setf result 1)
  (dotimes (i highest-number result)
    (setf result (* result
                    (expt i (aref factors-array i))))
  )
); }}}

(defun make-zeroed-array (array-size); {{{
  (setf an-array (make-array array-size))
  (dotimes (i array-size t)
    (setf (aref an-array i) 0)
  )
  an-array
); }}}

;The sum of the squares of the first ten natural numbers is, 1^(2) + 2^(2) + ...
;+ 10^(2) = 385
;
;The square of the sum of the first ten natural numbers is, (1 + 2 + ... +
;10)^(2) = 55^(2) = 3025
;
;Hence the difference between the sum of the squares of the first ten natural
;numbers and the square of the sum is 3025 − 385 = 2640.
;
;Find the difference between the sum of the squares of the first one hundred
;natural numbers and the square of the sum.

(defun project-euler-6-1 (); {{{
  (let*
    (
      (n 100)

      (sum-n (/ (* n (1+ n)) 2))
      (square-sum-n (expt sum-n 2))

      ; n(n + 1)(2n + 1)/6
      ; http://en.wikipedia.org/wiki/Sum
      (sum-squares (/ (* n (1+ n) (1+ (* n 2))) 6))
    )

    (- square-sum-n sum-squares)
  )

); }}}
