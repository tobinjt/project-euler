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

; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
; that the 6^(th) prime is 13.
;
; What is the 10001^(st) prime number?

; How about: an array of primes, initially containing 2; an infinite loop,
; testing the next number to see if it's a multiple of any of the primes, and if
; not, appending it to the array.  Keep going until the array is full.
(defun project-euler-7-1 (); {{{
  (let*
    (
      (number-of-primes 10001)
      (primes (make-zeroed-array number-of-primes))
      (first-prime 2)
      (next-prime-index 1)
      (current-number first-prime)
    )

    (setf (aref primes 0) first-prime)
    (do*
      (
      )
      (
        (equal number-of-primes next-prime-index)
      )

      (let*
        (
          (current-index 0)
          (current-prime (aref primes current-index))
          (max-divisor (sqrt current-number))
        )

        (loop
          (when (zerop current-prime)
            ; shouldn't happen; we ran off the end of the primes array
            (break "current-prime == 0")
          )
          (when (zerop (mod current-number current-prime))
            ; we found a divisor; current-number is not prime
            (return nil)
          )
          (when (> current-prime max-divisor)
            ; current-number is prime; save it, and move on to the next number
            (setf (aref primes next-prime-index) current-number)
            (setf next-prime-index (1+ next-prime-index))
            (return t)
          )
          (setf current-index (1+ current-index))
          (setf current-prime (aref primes current-index))
        )

        (setf current-number (1+ current-number))
      )
    )
    (aref primes (1- number-of-primes))
  )
); }}}


; Find the greatest product of five consecutive digits in the 1000-digit number.
;
; 73167176531330624919225119674426574742355349194934
; 96983520312774506326239578318016984801869478851843
; 85861560789112949495459501737958331952853208805511
; 12540698747158523863050715693290963295227443043557
; 66896648950445244523161731856403098711121722383113
; 62229893423380308135336276614282806444486645238749
; 30358907296290491560440772390713810515859307960866
; 70172427121883998797908792274921901699720888093776
; 65727333001053367881220235421809751254540594752243
; 52584907711670556013604839586446706324415722155397
; 53697817977846174064955149290862569321978468622482
; 83972241375657056057490261407972968652414535100474
; 82166370484403199890008895243450658541227588666881
; 16427171479924442928230863465674813919123162824586
; 17866458359124566529476545682848912883142607690042
; 24219022671055626321111109370544217506941658960408
; 07198403850962455444362981230987879927244284909188
; 84580156166097919133875499200524063689912560717606
; 05886116467109405077541002256983155200055935729725
; 71636269561882670428252483600823257530420752963450

(defun get-digit (a-string index)
  (parse-integer (string (char a-string index)))
)

(defun project-euler-8-1 (); {{{
  (let*
    (
      (the-number 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
      (the-number-string (write-to-string the-number))
      (number-of-digits 5)
      (digits-last-index (1- number-of-digits))
      (highest-product 1)
      (digits (make-string number-of-digits))
      (highest-digits (make-string number-of-digits))
    )

    ; populate digits and highest-digits
    (dotimes (i number-of-digits)
      (let
        (
          (digit (char the-number-string i))
        )
        (setf (char digits i) digit)
        (setf (char highest-digits i) digit)
        (setf highest-product (* (get-digit the-number-string i) highest-product))
      )
    )

    ; loop over the remaining digits in the-number-string
    (do
      (
        (i number-of-digits)
      )
      (
        (equal i (length the-number-string))
      )

      (let
        (
          (next-digit (get-digit the-number-string i))
          (product 1)
        )
        ; shift the current group of digits left
        (dotimes (j digits-last-index)
          (setf (char digits j) (char digits (1+ j)))
          (setf product (* product (get-digit digits j)))
        )
        ; append the next digit from the-number-string
        (setf (char digits digits-last-index) (char the-number-string i))
        (setf product (* product (get-digit digits digits-last-index)))
        (when (> product highest-product)
          ; update highest-product and highest-digits
          (setf highest-product product)
          (dotimes (j digits-last-index)
            (setf (char highest-digits j) (char digits j))
          )
        )
      )

      (setf i (1+ i))
    )
    (list highest-product highest-digits)
  )
); }}}
