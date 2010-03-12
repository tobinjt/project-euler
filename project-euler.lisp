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
      (current-factor (funcall possible-factors) (funcall possible-factors))
      (factors nil)
    )
    ((null current-factor) factors)

    (when (zerop (mod an-integer current-factor))
      (setf factors (cons current-factor factors))
      (setf factors (cons (/ an-integer current-factor) factors))
    )
  )
); }}}

(defun project-euler-3-1 (); {{{
  (let
    (
      (sorted-factors (sort (get-factors 600851475143) #'>))
    )

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
      (previous-number start)
    )
    (lambda ()
      (if (> previous-number end)
        nil
        (let
          (
            (result previous-number)
          )
          (setf previous-number (1+ previous-number))
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
      ((outer-number 999 (1- outer-number)))
      ((or (< outer-number 100)
            ; Stop when it's not possible to produce a product larger than the
            ; current palindrome.
           (< outer-number (/ palindrome outer-number))
      ))

      (do
        ((inner-number outer-number (1- inner-number)))
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
            (setf numbers (list outer-number inner-number))
          )
        )
      )

    )
    (cons palindrome numbers)
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
  (let*
    (
      (highest-number 20)
      ; I don't want to be constantly adding or subtracting 1 to indices.
      (array-size (1+ highest-number))
      ; This breaks the generalisation, but it could easily be replaced if
      ; necessary.
      (primes '(2 3 5 7 11 13 17 19))
      (factors-array (make-array array-size :initial-element 0))
    )

    (do
      (
        (current-number 2 (1+ current-number))
      )
      (
        (> current-number highest-number)
      )

      (let
        (
          (remainder current-number)
          (current-factors (make-array array-size :initial-element 0))
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
        (dotimes (i highest-number)
          (setf (aref factors-array i) (max
                                            (aref current-factors i)
                                            (aref factors-array i)
                                       )
          )
        )
      )
    )
    (let
      (
        (result 1)
      )

      (dotimes (i highest-number result)
        (setf result (* result
                        (expt i (aref factors-array i))))
      )
    )
  )
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
      (primes (make-array number-of-primes :initial-element 0))
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
        (i number-of-digits (1+ i))
      )
      (
        (equal i (length the-number-string))
      )

      (let
        (
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

    )
    (list highest-product highest-digits)
  )
); }}}

; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
; a^(2) + b^(2) = c^(2)
;
; For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
;
; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
; Find the product abc.

; a + b + c = 1000
; c = 1000 - a - b
; a**2 + b**2 = c**2
; c = sqrt(a**2 + b**2)
; 1000 - a - b = sqrt(a**2 + b**2)
; 1000 = sqrt(a**2 + b**2) + a + b
; a < b < c
; for (int a = 1; a < 500; a++) {
;   for (int b = a + 1; b <= 500; b++) {
;     if (sqrt(a**2 + b**2) + a + b == 1000) {
;       success!
;     }
;   }
; }
(defun project-euler-9-1 (); {{{
  (do
    (
      (a 1 (1+ a))
      (result nil)
    )
    (
      (or
        (not (null result))
        (>= a 500)
      )
      result
    )

    (do
      (
        (b a (1+ b))
      )
      (
        (or
          (not (null result))
          (> b 500)
        )
        result
      )

      (let
        (
          (value
            (+
              (sqrt (+ (expt a 2) (expt b 2)))
              a b
            )
          )
        )

        (when (equal value 1000)
          (setf result (list a b (- 1000 (+ a b))))
          (setf result
            (list
              (* a b (first (last result)))
              result
            )
          )
        )
      )
    )
  )
); }}}

; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;
; Find the sum of all the primes below two million.

; This attempt would have taken roughly 40 minutes, with garbage collection
; consuming 60-70% of that time.
(defun project-euler-10-1 (); {{{
  (do*
    (
      (primes '(2))
      (current-number (1+ (first primes)) (+ 2 current-number))
      (sum-of-primes (first primes))
      (sqrt-current-number (sqrt current-number) (sqrt current-number))
      (remaining-primes primes primes)
    )
    (
      (>= current-number 2000000)
      sum-of-primes
    )

    (loop
      (when (zerop (mod current-number (first remaining-primes)))
        ; We found a divisor; current-number is not prime
        (return nil)
      )
      (when (> (first remaining-primes) sqrt-current-number)
        ; We found a prime
        (setf primes (append primes (list current-number)))
        (setf sum-of-primes (+ sum-of-primes current-number))
        (return t)
      )
      (setf remaining-primes (rest remaining-primes))
    )
  )
); }}}

(defun project-euler-10-2 (); {{{
  (let
    (
      (primes (sieve-of-eratosthenes 2000000))
      (sum-of-primes 0)
    )
    (dotimes (i (length primes) sum-of-primes)
      (when (aref primes i)
        (setf sum-of-primes (+ sum-of-primes i))
      )
    )
  )
); }}}

(defun sieve-of-eratosthenes (upper-bound); {{{
  (let*
    (
      (array-size (1+ upper-bound))
      (primes (make-array array-size :initial-element t))
    )
    (setf (aref primes 0) nil)
    (setf (aref primes 1) nil)

    (dotimes (i (1+ (ceiling (sqrt array-size))) primes)
      (when (aref primes i)
        (do
          (
            (index-of-multiples (expt i 2) (+ index-of-multiples i))
          )
          (
            (>= index-of-multiples array-size)
          )

          (setf (aref primes index-of-multiples) nil)
        )
      )
    )
  )
); }}}

(defun seq-list (lower-bound upper-bound); {{{
  (let
    (
      (current-number lower-bound)
      (result '())
    )

    (loop
      (when (> current-number upper-bound)
        (return (reverse result))
      )
      (push result current-number)
      (setf current-number (1+ current-number))
    )
  )
); }}}

(defun seq-list2 (min max)
  (loop for i from min to max collect i)
)

; ------------------ {{{
; Someone else's LISP solution for Project Euler problem 10.
;(defun seq-list (min max)
;  (loop for i from min to max collect i)
;)
;
;(defun sieve (lst)
;  (let
;    (
;      (primes '())
;      (last (car (last lst)))
;    )
;    (loop while (and lst (> last (* (car lst) (car lst))))
;      do
;        (let
;          (
;            (factor (car lst))
;          )
;          (setq primes (cons factor primes))
;          (setq lst
;            (remove-if
;              #'(lambda (n) (= (mod n factor) 0))
;              (cdr lst))
;            )
;          )
;        )
;      (append (reverse primes) lst)
;    )
;  )
;
;(defun all-primes (limit)
;  (sieve (seq-list 2 limit))
;)
;
;(defun euler10 ()
;  (reduce #'+ (all-primes 1000000))
;)
;------------------- }}}

; 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;
; What is the sum of the digits of the number 2^(1000)?
(defun project-euler-16-1 (); {{{
  (let
    (
      (number-string (write-to-string (expt 2 1000)))
      (sum-of-digits 0)
    )

    (dotimes (i (length number-string) sum-of-digits)
      (setf sum-of-digits (+ sum-of-digits
                             (parse-integer (string (char number-string i)))
                          )
      )
    )
  )
); }}}

; In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
; 
; 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
; 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
; 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
; 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
; 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
; 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
; 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
; 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
; 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
; 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
; 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
; 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
; 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
; 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
; 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
; 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
; 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
; 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
; 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
; 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
; 
; The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
; 
; What is the greatest product of four adjacent numbers in any direction (up,
; down, left, right, or diagonally) in the 20×20 grid?
(defun project-euler-11-1 ()
  (let
    (
      (matrix (make-array '(20 20)
                          :initial-contents
                          '(
                            (08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
                            (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
                            (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
                            (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
                            (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
                            (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
                            (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
                            (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
                            (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
                            (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
                            (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
                            (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
                            (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
                            (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
                            (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
                            (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
                            (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
                            (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
                            (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
                            (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)
                          )
              )
      )
    )
  )
)
