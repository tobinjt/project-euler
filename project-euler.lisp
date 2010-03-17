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
  ; The tests don't work for numbers <= 3
  (case an-integer
    ((0 1) nil)
    ((2 3) t)
    (otherwise 
      ; This doesn't work for negative numbers.
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

(defun project-euler-11-1 (); {{{
  (labels ((calculate-product (matrix i j offset)
              (let ((product 1)
                    (x-offset (first offset))
                    (y-offset (second offset)))
                (dotimes (n 4 product)
                  (let ((x (+ i (* n x-offset)))
                        (y (+ j (* n y-offset))))
                    (if (array-in-bounds-p matrix x y)
                        (setf product (* product (aref matrix x y)))
                        (setf product 0)))))))

    (let ((max_product 0)
          (max_coords nil)
          (max_offset nil)
          (offsets '((0 1)      ; right along the row
                     (1 0)      ; down along the column
                     (1 1)      ; south-east diagonal
                     (1 -1)))   ; south-west diagonal
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
                                (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))))

      (dotimes (i (array-dimension matrix 0) (list max_product max_coords max_offset))
        (dotimes (j (array-dimension matrix 1))
          (dolist (offset offsets)
            (let ((product (calculate-product matrix i j offset)))
              (when (> product max_product)
                (setf max_product product
                      max_coords  (list i j)
                      max_offset  offset))))))))); }}}

; The sequence of triangle numbers is generated by adding the natural numbers.
; So the 7^(th) triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The
; first ten terms would be:
;
; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;
; Let us list the factors of the first seven triangle numbers:
;
;      1: 1
;      3: 1,3
;      6: 1,2,3,6
;     10: 1,2,5,10
;     15: 1,3,5,15
;     21: 1,3,7,21
;     28: 1,2,4,7,14,28
;
; We can see that 28 is the first triangle number to have over five divisors.
;
; What is the value of the first triangle number to have over five hundred
; divisors?

; Musings: must be at least sum-n(500); must be divisible by 2 and 3 (not being
; so rules out too many factors);
; Possible solutions: could do trial division from 1 to sqrt(x).
; Efficient solution: Find the factors of n/2 and n+1, then multiply them
; together.

(defun project-euler-12-1 (); {{{
  "Trial division solution to Project Euler 12"
  (do* ((current-number 500 (1+ current-number))
        (sum (/ (* current-number (1+ current-number)) 2)
             (+ sum current-number))
        (sqrt-sum (sqrt sum) (sqrt sum))
        (num-factors 2)) ; 1 and itself
                         ; XXX why does the loop break if I add 2 above?  It
                         ; looks like num-factors is reset before the loop
                         ; conditional is evaluated.
       ((> num-factors 500) (list (1- current-number) (- sum current-number) num-factors))
    ; Inlining this reduces run time by 60ish%.
    (setf num-factors 2)
    (do ((current-factor 2 (1+ current-factor)))
        ((> current-factor sqrt-sum) )
      (when (zerop (mod sum current-factor))
        (incf num-factors 2))))
); }}}

(defun factors (n); {{{
  (let ((factors nil))
    (do ((i 1 (1+ i))
        (max-divisor (sqrt n)))
        ((> i max-divisor) (sort factors '<))
      (when (zerop (mod n i))
        (push i factors)
        (when (not (= i (/ n i)))
          (push (/ n i) factors)))))); }}}

(defun project-euler-12-2 (); {{{
  "More efficient solution to Project Euler 12"
  (let ((primes '(2))
        (num-factors-of-n-1 0)
        (num-factors-of-n 2))
    (do* ((n 2 (1+ n))
          (n-to-divide))
        ((> (* num-factors-of-n-1 num-factors-of-n) 500)
         ; I expect n to be one greater than it should be, but it appears to be
         ; two greater, and I don't know why.
         (list (- n 2) (/ (* (- n 2) (1- n)) 2)))
      (setf num-factors-of-n-1 num-factors-of-n
            num-factors-of-n 1)
      (if (evenp n)
          (setf n-to-divide (/ n 2))
          (setf n-to-divide n))
      (dolist (prime primes)
        (let ((exponent 0))
          (do ()
              ((or (> prime n-to-divide)
                   (not (zerop (mod n-to-divide prime)))))
            (setf n-to-divide (/ n-to-divide prime)
                  exponent    (1+ exponent)))
          (when (not (zerop exponent))
            (setf num-factors-of-n (* num-factors-of-n (+ exponent 1))))))
      (when (not (= 1 n-to-divide))
        ; n-to-divide is a new prime
        (setf primes (append primes (list n-to-divide))))))
); }}}

(defun project-euler-12-3 (); {{{
  "More efficient trial division solution to Project Euler 12"
  (do ((n 2)
       (num-factors 0)
       (num-factors-n 0)
       ; The initial value is wrong, but the first loop iteration will correct
       ; it.
       (num-factors-n-1 0))
      ((> num-factors 500)
       (progn (decf n) (list n (/ (* n (1+ n)) 2) num-factors)))

    (setf num-factors-n-1 num-factors-n
          num-factors-n 0
          n (1+ n))
    (do* ((divisor 1)
          (n-to-divide (if (evenp n) (/ n 2) n))
          (max-divisor (1+ (ceiling (sqrt n-to-divide)))))
         ((> divisor max-divisor))
      (when (zerop (mod n-to-divide divisor))
        (incf num-factors-n)
        (when (not (= divisor (/ n-to-divide divisor)))
          (incf num-factors-n)))
      (incf divisor))
    (setf num-factors (* num-factors-n num-factors-n-1)))
); }}}

; Work out the first ten digits of the sum of the following one-hundred 50-digit
; numbers.

(defun project-euler-13-1 (); {{{
  (subseq (write-to-string (reduce #'+ '(
                      37107287533902102798797998220837590246510135740250; {{{
                      46376937677490009712648124896970078050417018260538
                      74324986199524741059474233309513058123726617309629
                      91942213363574161572522430563301811072406154908250
                      23067588207539346171171980310421047513778063246676
                      89261670696623633820136378418383684178734361726757
                      28112879812849979408065481931592621691275889832738
                      44274228917432520321923589422876796487670272189318
                      47451445736001306439091167216856844588711603153276
                      70386486105843025439939619828917593665686757934951
                      62176457141856560629502157223196586755079324193331
                      64906352462741904929101432445813822663347944758178
                      92575867718337217661963751590579239728245598838407
                      58203565325359399008402633568948830189458628227828
                      80181199384826282014278194139940567587151170094390
                      35398664372827112653829987240784473053190104293586
                      86515506006295864861532075273371959191420517255829
                      71693888707715466499115593487603532921714970056938
                      54370070576826684624621495650076471787294438377604
                      53282654108756828443191190634694037855217779295145
                      36123272525000296071075082563815656710885258350721
                      45876576172410976447339110607218265236877223636045
                      17423706905851860660448207621209813287860733969412
                      81142660418086830619328460811191061556940512689692
                      51934325451728388641918047049293215058642563049483
                      62467221648435076201727918039944693004732956340691
                      15732444386908125794514089057706229429197107928209
                      55037687525678773091862540744969844508330393682126
                      18336384825330154686196124348767681297534375946515
                      80386287592878490201521685554828717201219257766954
                      78182833757993103614740356856449095527097864797581
                      16726320100436897842553539920931837441497806860984
                      48403098129077791799088218795327364475675590848030
                      87086987551392711854517078544161852424320693150332
                      59959406895756536782107074926966537676326235447210
                      69793950679652694742597709739166693763042633987085
                      41052684708299085211399427365734116182760315001271
                      65378607361501080857009149939512557028198746004375
                      35829035317434717326932123578154982629742552737307
                      94953759765105305946966067683156574377167401875275
                      88902802571733229619176668713819931811048770190271
                      25267680276078003013678680992525463401061632866526
                      36270218540497705585629946580636237993140746255962
                      24074486908231174977792365466257246923322810917141
                      91430288197103288597806669760892938638285025333403
                      34413065578016127815921815005561868836468420090470
                      23053081172816430487623791969842487255036638784583
                      11487696932154902810424020138335124462181441773470
                      63783299490636259666498587618221225225512486764533
                      67720186971698544312419572409913959008952310058822
                      95548255300263520781532296796249481641953868218774
                      76085327132285723110424803456124867697064507995236
                      37774242535411291684276865538926205024910326572967
                      23701913275725675285653248258265463092207058596522
                      29798860272258331913126375147341994889534765745501
                      18495701454879288984856827726077713721403798879715
                      38298203783031473527721580348144513491373226651381
                      34829543829199918180278916522431027392251122869539
                      40957953066405232632538044100059654939159879593635
                      29746152185502371307642255121183693803580388584903
                      41698116222072977186158236678424689157993532961922
                      62467957194401269043877107275048102390895523597457
                      23189706772547915061505504953922979530901129967519
                      86188088225875314529584099251203829009407770775672
                      11306739708304724483816533873502340845647058077308
                      82959174767140363198008187129011875491310547126581
                      97623331044818386269515456334926366572897563400500
                      42846280183517070527831839425882145521227251250327
                      55121603546981200581762165212827652751691296897789
                      32238195734329339946437501907836945765883352399886
                      75506164965184775180738168837861091527357929701337
                      62177842752192623401942399639168044983993173312731
                      32924185707147349566916674687634660915035914677504
                      99518671430235219628894890102423325116913619626622
                      73267460800591547471830798392868535206946944540724
                      76841822524674417161514036427982273348055556214818
                      97142617910342598647204516893989422179826088076852
                      87783646182799346313767754307809363333018982642090
                      10848802521674670883215120185883543223812876952786
                      71329612474782464538636993009049310363619763878039
                      62184073572399794223406235393808339651327408011116
                      66627891981488087797941876876144230030984490851411
                      60661826293682836764744779239180335110989069790714
                      85786944089552990653640447425576083659976645795096
                      66024396409905389607120198219976047599490197230297
                      64913982680032973156037120041377903785566085089252
                      16730939319872750275468906903707539413042652315011
                      94809377245048795150954100921645863754710598436791
                      78639167021187492431995700641917969777599028300699
                      15368713711936614952811305876380278410754449733078
                      40789923115535562561142322423255033685442488917353
                      44889911501440648020369068063960672322193204149535
                      41503128880339536053299340368006977710650566631954
                      81234880673210146739058568557934581403627822703280
                      82616570773948327592232845941706525094512325230608
                      22918802058777319719839450180888072429661980811197
                      77158542502016545090413245809786882778948721859617
                      72107838435069186155435662884062257473692284509516
                      20849603980134001723930671666823555245252804609722
                      53503534226472524250874054075591789781264330331690; }}}
          ))) 0 10)
); }}}

; The following iterative sequence is defined for the set of positive integers:
; 
; n  n/2 (n is even)
; n  3n + 1 (n is odd)
; 
; Using the rule above and starting with 13, we generate the following sequence:
; 
; 13  40  20  10  5  16  8  4  2  1
; 
; It can be seen that this sequence (starting at 13 and finishing at 1) contains
; 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
; that all starting numbers finish at 1.
; 
; Which starting number, under one million, produces the longest chain?
; 
; NOTE: Once the chain starts the terms are allowed to go above one million.

; Thoughts: Array of one million elements, each will hold the number of terms in
; the chain for that number.  Also a queue, to hold numbers whose count we don't
; know.  Pick any starting number, push it onto the queue, and follow its chain,
; pushing each subsequent number onto the queue; when the chain ends, start
; working through the queue, counting the number of steps, and saving that
; number in the array.  Pick the lowest number whose chain hasn't been
; calculated, and repeat the pattern, short-circuiting if a number exists in the
; array.  When there are no remaining numbers in the array, find the number with
; the longest chain.
