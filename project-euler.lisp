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




; Create a closure that will return the next Fibonacci number each time it's; {{{
; called.  I guessed how to write a closure in LISP; I'd prefer to create an
; anonymous closure at the start of project-euler-2-1, but I haven't figured
; that out yet - I think it's several chapters away in my LISP book.; }}}
(let; {{{
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
); }}}

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

; The prime factors of 13195 are 5, 7, 13 and 29.; {{{
;
; What is the largest prime factor of the number 600851475143 ?

; get the factors of 600851475143
; test each one for primeness, starting with the largest; }}}

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


; A palindromic number reads the same both ways. The largest palindrome made; {{{
; from the product of two 2-digit numbers is 9009 = 91 × 99.
;
; Find the largest palindrome made from the product of two 3-digit numbers.; }}}
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

; 2520 is the smallest number that can be divided by each of the numbers from 1; {{{
; to 10 without any remainder.
;
; What is the smallest number that is evenly divisible by all of the numbers
; from 1 to 20?

; Find the prime factors of each number between 1 and 20, including the power,
; so 9 = 3**2.  The smallest number is the product of the highest prime factors.; }}}

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

;The sum of the squares of the first ten natural numbers is, 1^(2) + 2^(2) + ...; {{{
;+ 10^(2) = 385
;
;The square of the sum of the first ten natural numbers is, (1 + 2 + ... +
;10)^(2) = 55^(2) = 3025
;
;Hence the difference between the sum of the squares of the first ten natural
;numbers and the square of the sum is 3025 − 385 = 2640.
;
;Find the difference between the sum of the squares of the first one hundred
;natural numbers and the square of the sum.; }}}

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

; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see; {{{
; that the 6^(th) prime is 13.
;
; What is the 10001^(st) prime number?

; How about: an array of primes, initially containing 2; an infinite loop,
; testing the next number to see if it's a multiple of any of the primes, and if
; not, appending it to the array.  Keep going until the array is full.; }}}
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

; Find the greatest product of five consecutive digits in the 1000-digit number.; {{{
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
; 71636269561882670428252483600823257530420752963450; }}}

(defun get-digit (a-string index); {{{
  (parse-integer (string (char a-string index)))
); }}}

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

; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,; {{{
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
; }; }}}
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

; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.; {{{
;
; Find the sum of all the primes below two million.

; This attempt would have taken roughly 40 minutes, with garbage collection
; consuming 60-70% of that time.; }}}
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

(defun seq-list2 (min max); {{{
  (loop for i from min to max collect i)
); }}}

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

; 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.; {{{
;
; What is the sum of the digits of the number 2^(1000)?; }}}
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

; In the 20×20 grid below, four numbers along a diagonal line have been marked in red.; {{{
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
; down, left, right, or diagonally) in the 20×20 grid?; }}}

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

; The sequence of triangle numbers is generated by adding the natural numbers.; {{{
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
; together.; }}}

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

; Work out the first ten digits of the sum of the following one-hundred 50-digit; {{{
; numbers.; }}}

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

; The following iterative sequence is defined for the set of positive integers:; {{{
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
; the longest chain.; }}}

(defun project-euler-14-1 (); {{{
  (let ((array-size 1000000))
    (let ((cache (make-array array-size))
          (chain-end 1)
          (chain-start 1))
      (setf (aref cache chain-end) 1)
      (do ((n 2 (1+ n))
           (queue '() '()))
          ((>= n array-size) chain-start)

        (unless (aref cache n)
          ; Fill up the queue.
          (do ((val n))
              ((= val chain-end) (push chain-end queue))
            (push val queue)
            ; Break out of the loop if we've seen this value before.
            (if (and (array-in-bounds-p cache val)
                     (aref cache val))
                (return))
            (if (evenp val)
                (setf val (/ val 2))
                (setf val (1+ (* val 3)))))

          ; Empty the queue
          (let ((chain-length (aref cache (pop queue))))
            (dolist (val queue)
              (incf chain-length)
              (when (array-in-bounds-p cache val)
                (setf (aref cache val) chain-length))
              (when (and (array-in-bounds-p cache val)
                         (> chain-length (aref cache chain-start)))
                (setf chain-start val))))))))
); }}}

; Starting in the top left corner of a 2x2 grid, there are 6 routes (without; {{{
; backtracking) to the bottom right corner.
;
; How many routes are there through a 20x20 grid?

; http://blog.functionalfun.net/2008/07/project-euler-problem-15-city-grids-and.html; }}}
(defun project-euler-15-1 (&key (x 20) (y 20)); {{{
  (let ((result 1)
        (row (* 2 x)))
    (dotimes (column (1+ y) result)
      (when (not (zerop column))
        ; result = (result * (row + 1 - i)) / i;
        (setf result (/ (* result (- (1+ row) column)) column))
        )))
); }}}

; If the numbers 1 to 5 are written out in words: one, two, three, four, five,; {{{
; then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
;
; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
; words, how many letters would be used?
;
; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
; forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
; letters. The use of "and" when writing out numbers is in compliance with
; British usage.; }}}

(defun project-euler-17-1 (); {{{
  (labels ((count-letters (list-of-words)
                          (reduce #'+ (mapcar #'length list-of-words))))
    (let* ((num-decades         8)
           (num-hundreds        9)
           (num-hundred-ands    99)
           (sum-below-10        (count-letters '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
           (sum-teens           (count-letters '("ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")))
           (sum-decades         (count-letters '("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety")))
           (sum-20-99           (+ (* sum-decades 10) (* sum-below-10 num-decades)))
           (sum-below-100       (+ sum-below-10 sum-teens sum-20-99))
           (sum-x-hundred-ands  (* num-hundred-ands (+ sum-below-10 (* num-hundreds (count-letters '("hundred" "and"))))))
           (sum-x-hundred       (+ sum-below-10 (* num-hundreds (count-letters '("hundred")))))
           (sum-100-999         (+ sum-x-hundred sum-x-hundred-ands (* num-hundreds sum-below-100)))
           (sum-1-1000          (+ (count-letters '("one" "thousand")) sum-below-100 sum-100-999)))
      (format t "sum-below-10 ~A~% sum-teens ~A~% sum-decades ~A~% sum-20-99 ~A~% sum-below-100 ~A~% sum-x-hundred-ands ~A~% sum-100-999 ~A~% "
                 sum-below-10      sum-teens      sum-decades      sum-20-99      sum-below-100      sum-x-hundred-ands      sum-100-999)
      sum-1-1000))
); }}}

; n! means n  (n  1)  ...  3  2  1; {{{
;
; Find the sum of the digits in the number 100!; }}}

(defun project-euler-20-1 (); {{{
  (reduce #'+ (mapcar #'(lambda (x) (parse-integer (string x)))
                      (coerce (write-to-string (reduce #'* (loop for i from 1 to 100 collect i))) 'list)))
); }}}

; By starting at the top of the triangle below and moving to adjacent numbers on; {{{
; the row below, the maximum total from top to bottom is 23.
;
; 3
; 7 4
; 2 4 6
; 8 5 9 3
;
; That is, 3 + 7 + 4 + 9 = 23.
;
; Find the maximum total from top to bottom of the triangle below:
;
; 75
; 95 64
; 17 47 82
; 18 35 87 10
; 20 04 82 47 65
; 19 01 23 75 03 34
; 88 02 77 73 07 63 67
; 99 65 04 28 06 16 70 92
; 41 41 26 56 83 40 80 70 33
; 41 48 72 33 47 32 37 16 94 29
; 53 71 44 65 25 43 91 52 97 51 14
; 70 11 33 28 77 73 17 78 39 68 17 57
; 91 71 52 38 17 14 91 43 58 50 27 29 48
; 63 66 04 68 89 53 67 30 73 16 69 87 40 31
; 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
;
; NOTE: As there are only 16384 routes, it is possible to solve this problem by
; trying every route. However, Problem 67, is the same challenge with a triangle
; containing one-hundred rows; it cannot be solved by brute force, and requires
; a clever method! ;o); }}}

(defstruct a-path path total)
(defun project-euler-18-1 (); {{{
  ; First create the data structures
  (let* ((triangle-list '((75)
                          (95 64)
                          (17 47 82)
                          (18 35 87 10)
                          (20 04 82 47 65)
                          (19 01 23 75 03 34)
                          (88 02 77 73 07 63 67)
                          (99 65 04 28 06 16 70 92)
                          (41 41 26 56 83 40 80 70 33)
                          (41 48 72 33 47 32 37 16 94 29)
                          (53 71 44 65 25 43 91 52 97 51 14)
                          (70 11 33 28 77 73 17 78 39 68 17 57)
                          (91 71 52 38 17 14 91 43 58 50 27 29 48)
                          (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
                          (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))
        (triangle-array  (make-array (list (length triangle-list) (length triangle-list))))
        (triangle-summed (make-array (list (length triangle-list) (length triangle-list)))))
    (dolist (row-contents triangle-list)
      (let ((i (1- (length row-contents)))
            (j 0))
        (dolist (value row-contents)
          (setf (aref triangle-array i j) value)
          (incf j))))
    (setf (aref triangle-summed 0 0 ) (make-a-path :path '((0 0)) :total (aref triangle-array 0 0)))

    ; Now solve the problem.

    ; Don't process the last row: that's where the results will be.
    ; Don't process the last column: it's nil in every row except the last.
    ; Those conditions mean we can't exceed the array dimensions, so we don't
    ; need to bounds-check every array access.
    ; Each node will create its right child node, choosing either itself or the
    ; node to its right as the better parent.  The first node in each row is
    ; also responsible for creating its left child.
    (dotimes (i (1- (array-dimension triangle-array 0)))
      (dotimes (j (1- (array-dimension triangle-array 1)))
        (let* ((i+1 (1+ i))
               (j+1 (1+ j))
               (current-node (aref triangle-summed i j))
               (right-node   (aref triangle-summed i j+1))
               (better-node  current-node)
               (below-total  (aref triangle-array  i+1 j+1)))

          (when (= j 0)
            ; This node is the start of the row, and so must be the parent of
            ; the left child.
            (setf (aref triangle-summed i+1 0)
                  (make-a-path :total (+ (aref triangle-array i+1 0) (a-path-total current-node))
                               :path  (cons (list i+1 0) (a-path-path current-node)))))

          ; Skip the upper right half of the triangle
          (when current-node
            (when (and right-node
                      (> (a-path-total right-node) (a-path-total current-node))
              (setf better-node right-node)))

            (setf (aref triangle-summed i+1 j+1)
                  (make-a-path :total (+ below-total (a-path-total better-node))
                              :path  (cons (list i+1 j+1) (a-path-path better-node))))))))

    (let ((result (aref triangle-summed 0 0))
          (i (1- (array-dimension triangle-summed 0))))
      (dotimes (j (array-dimension triangle-summed 1) result)
        (when (> (a-path-total (aref triangle-summed i j)) (a-path-total result))
          (setf result (aref triangle-summed i j)))))
    )); }}}

; You are given the following information, but you may prefer to do some; {{{
; research for yourself.
;
; 1 Jan 1900 was a Monday.
; Thirty days has September,
; April, June and November.
; All the rest have thirty-one,
; Saving February alone,
; Which has twenty-eight, rain or shine.
; And on leap years, twenty-nine.
; A leap year occurs on any year evenly divisible by 4, but not on a century
; unless it is divisible by 400.
; How many Sundays fell on the first of the month during the twentieth century
; (1 Jan 1901 to 31 Dec 2000)?; }}}

(defun is-leap-year (year); {{{
  (or (zerop (mod year 400))
      (and (not (zerop (mod year 100)))
          (zerop (mod year 4))))); }}}

(defun days-in-year (year); {{{
  (if (is-leap-year year)
      366
      365)); }}}

(let ((days-in-month (make-array 13 :initial-element 31))); {{{
  (mapcar #'(lambda (x) (setf (aref days-in-month x) 30))
          '(4 6 9 11))
  (setf (aref days-in-month 2) 28)
  (defun days-in-month (year month)
    (if (and (= month 2)
               (is-leap-year year))
        29
        (aref days-in-month month)))); }}}

(defun days-to-ymd (days); {{{
  (let ((year 1900)
        (month 1))

    (do ()
        ((< days (days-in-year year)))
      (decf days (days-in-year year))
      (incf year))

    (do ()
        ((< days (days-in-month year month)))
      (decf days (days-in-month year month))
      (incf month))

    (values year month days))); }}}

(defun project-euler-19-1 (); {{{
  (let ((result 0)
        ; 1900/01/07 is the first Sunday, and we skip all of 1900
        (sunday (+ 7 (* 7 52))))
    (loop
      (multiple-value-bind (year month day) (days-to-ymd sunday)
        (declare (ignore month))
        (when (> year 2000)
          (return result))
        (when (= day 1)
          (incf result))
        (incf sunday 7))))
); }}}

;Let d(n) be defined as the sum of proper divisors of n (numbers less than n; {{{
;which divide evenly into n).  If d(a) = b and d(b) = a, where a != b, then a
;and b are an amicable pair and each of a and b are called amicable numbers.
;
;For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
;and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
;142; so d(284) = 220.
;
;Evaluate the sum of all the amicable numbers under 10000.; }}}

(defun project-euler-21-1 (); {{{
  (let ((sum-of-factors (make-array 10000 :initial-element 0)))
    ; Calculate the sum of factors for each number.
    (do ((current-number 1 (1+ current-number))
         (max-number (array-dimension sum-of-factors 0)))
        ((= current-number max-number))
      (do ((current-divisor 1 (1+ current-divisor))
           (max-divisor (floor (sqrt current-number))))
          ((> current-divisor max-divisor))
        (when (zerop (mod current-number current-divisor))
          (incf (aref sum-of-factors current-number) current-divisor)
          (let ((other-factor (/ current-number current-divisor)))
            (when (and (not (= current-divisor other-factor))
                       (not (= current-number other-factor)))
              (incf (aref sum-of-factors current-number) other-factor))))))
    ; Now look for amicable pairs.
    (do ((current-number 1 (1+ current-number))
         (max-number (array-dimension sum-of-factors 0))
         (sum-of-amicable-pairs 0))
        ((= current-number max-number) sum-of-amicable-pairs)
      (let ((sum-for-current-number (aref sum-of-factors current-number)))
        (when (and (< sum-for-current-number max-number)
                   ; Only count each pair once, e.g: 220 & 284; the pair will be
                   ; found when (= current-number 220), so skip when
                   ; (= current-number 284).
                   (> sum-for-current-number current-number)
                   (= current-number (aref sum-of-factors sum-for-current-number)))
          (incf sum-of-amicable-pairs (+ current-number sum-for-current-number))))))); }}}

; Using names.txt (right click and 'Save Link/Target As...'), a 46K text file; {{{
; containing over five-thousand first names, begin by sorting it into
; alphabetical order. Then working out the alphabetical value for each name,
; multiply this value by its alphabetical position in the list to obtain a name
; score.
;
; For example, when the list is sorted into alphabetical order, COLIN, which is
; worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
; would obtain a score of 938  53 = 49714.
;
; What is the total of all the name scores in the file?; }}}

(let ((char-values (make-hash-table)); {{{
      (value 0))
  (map nil #'(lambda (x) (setf (gethash x char-values) (incf value)))
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (defun count-string (a-string)
    (reduce #'+
            (map 'list
                 #'(lambda (x) (gethash x char-values))
                 a-string)))); }}}

(defun project-euler-22-1 (); {{{
  (let ((names-string (with-open-file (names #p"names.txt")
                        (read-line names))))
    (loop for i from 0 to (1- (length names-string)) do
      (when (equal #\, (char names-string i))
        (setf (char names-string i) #\ )))
    (let ((names-list (sort (read-from-string (concatenate 'string "(" names-string ")")) #'string<=))
          (current-position 0)
          (total 0))
      (dolist (name names-list total)
        (incf total (* (count-string name) (incf current-position))))))); }}}

; A perfect number is a number for which the sum of its proper divisors is; {{{
; exactly equal to the number. For example, the sum of the proper divisors of 28
; would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
;
; A number n is called deficient if the sum of its proper divisors is less than
; n and it is called abundant if this sum exceeds n.
;
; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
; number that can be written as the sum of two abundant numbers is 24. By
; mathematical analysis, it can be shown that all integers greater than 28123
; can be written as the sum of two abundant numbers. However, this upper limit
; cannot be reduced any further by analysis even though it is known that the
; greatest number that cannot be expressed as the sum of two abundant numbers is
; less than this limit.
;
; Find the sum of all the positive integers which cannot be written as the sum
; of two abundant numbers.; }}}

(defun factorise (a-number); {{{
  (let ((factors '()))
    (loop for current-divisor from 1 to (floor (sqrt a-number)) do
      (when (zerop (mod a-number current-divisor))
        (push current-divisor factors)
        (let ((other-factor (/ a-number current-divisor)))
          (when (and (not (= current-divisor other-factor))
                     (not (= a-number other-factor)))
            (push other-factor factors)))))
    (sort factors #'<))); }}}

(defun sum-factors (a-number); {{{
  (reduce #'+ (factorise a-number))); }}}

(defun project-euler-23-1 (); {{{
  (let* ((final-number 28123)
         (sum 0)
         (is-abundant (make-array (1+ final-number))))
    (loop for a-number from 1 to final-number do
      (setf (aref is-abundant a-number) (> (sum-factors a-number) a-number))
      (let ((sum-found nil))
        (loop for b-number from 1 to (/ a-number 2) until sum-found do
          (when (aref is-abundant b-number)
            (setf sum-found (aref is-abundant (- a-number b-number)))))
        (when (not sum-found)
          (incf sum a-number))))
    sum)); }}}

; The Fibonacci sequence is defined by the recurrence relation:; {{{
;     Fn = Fn-1 + Fn-2, where F1 = 1 and F2 = 1.
; Hence the first 12 terms will be:
;
; F1 = 1
; F2 = 1
; F3 = 2
; F4 = 3
; F5 = 5
; F6 = 8
; F7 = 13
; F8 = 21
; F9 = 34
; F10 = 55
; F11 = 89
; F12 = 144
; The 12th term, F12, is the first term to contain three digits.
;
; What is the first term in the Fibonacci sequence to contain 1000 digits?; }}}

(defun project-euler-25-1 (); {{{
  (let ((fib-2 1)
        (fib-1 1)
        (fib 0)
        ; The first loop iteration will caclulate F3.
        (num-terms 2)
        (greater-than (expt 10 999)))
    (loop while (< fib greater-than) do
      (setf fib       (+ fib-1 fib-2)
            fib-2     fib-1
            fib-1     fib
            num-terms (1+ num-terms)))
    num-terms)); }}}

; A permutation is an ordered arrangement of objects. For example, 3124 is one; {{{
; possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
; are listed numerically or alphabetically, we call it lexicographic order. The
; lexicographic permutations of 0, 1 and 2 are:
;
; 012   021   102   120   201   210
;
; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
; 5, 6, 7, 8 and 9?; }}}

(defun permute (a-list); {{{
  (if (not (second a-list))
      ; Terminating condition; return the list, wrapped in a new list, so that
      ; (cons) will do the right thing with the result.
      ; (cons 'x 'y) gives '(X . Y), whereas (cons 'x '(y)) gives '(X Y).
      (list a-list)
      (let ((used '())
            (unused (copy-list a-list))
            (current-item)
            (permutations '()))
        (loop while (first unused) do
          (setf current-item (pop unused))
          (mapcar #'(lambda (x) (setf permutations
                                      (cons (cons current-item x)
                                            permutations)))
                  (permute (append used unused)))
          (push current-item used))
        permutations))); }}}

(defun compare-permutations (x y); {{{
  (if (not (listp x))
      (< x y)
      (if (equal (first x) (first y))
          (compare-permutations (rest x) (rest y))
          (< (first x) (first y))))); }}}

(defun sort-permutations (permutations); {{{
  (sort permutations #'compare-permutations)); }}}

; This approach brute forces the permutations.
; (2 7 8 3 9 1 5 4 6 0)
(defun project-euler-24-1 (); {{{
  (nth 999999 (sort-permutations (permute '(0 1 2 3 4 5 6 7 8 9))))); }}}

(defun find-permutation (wanted-index items); {{{
  "Find permutation number wanted-index of the *sorted* list items"
  (if (= 1 (length items))
      items
      (let* ((num-items (length items))
             (factorial (reduce #'* (loop for i from 1 to (1- num-items) collect i)))
             (item-index (floor (/ wanted-index factorial)))
             (new-wanted-index (- wanted-index (* item-index factorial)))
             (i 0)
             (shorter-list '()))
        (dolist (item items)
          (unless (= i item-index)
            (push item shorter-list))
          (incf i))
        (append (list (nth item-index items))
                (find-permutation new-wanted-index (reverse shorter-list)))))); }}}

; This approach figures the answer out cleverly.
(defun project-euler-24-2 (); {{{
  (find-permutation 999999 '(0 1 2 3 4 5 6 7 8 9))); }}}

; A unit fraction contains 1 in the numerator. The decimal representation of the; {{{
; unit fractions with denominators 2 to 10 are given:
;
; 1/2 =   0.5
; 1/3 =   0.(3)
; 1/4 =   0.25
; 1/5 =   0.2
; 1/6 =   0.1(6)
; 1/7 =   0.(142857)
; 1/8 =   0.125
; 1/9 =   0.(1)
; 1/10  =   0.1
; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
; seen that 1/7 has a 6-digit recurring cycle.
;
; Find the value of d  1000 for which 1/d contains the longest recurring cycle
; in its decimal fraction part.

; Maybe: push 1/n onto a queue, then process each one; if the sequence repeats
; internally, push the repeated subsequences onto the queue and discard the
; current sequence; if there are no internally repeated subsequences, push the
; sequence onto a list to check later; when all sequences on the queue have been
; processed, take the longest sequence off the list.; }}}

(defstruct trie; {{{
  (num 0)
  (children (make-array 10))); }}}

(defun project-euler-26-1 (); {{{
  (let ((longest-sequence '())
        (length-longest-sequence 0)
        (longest-sequence-divisor 0)
        (longest-sequence-count))
    (loop for num from 2 to 999 do ; XXX
      (let ((reversed-digit-list (rational-to-reversed-list-of-digits (/ 1 num))))
        ; Strip repeated digits from the end of the list.
        (do ()
            ((not (and (second reversed-digit-list)
                       (= (first reversed-digit-list) (second reversed-digit-list)))))
          (pop reversed-digit-list))
        (let ((root (populate-trie (reverse reversed-digit-list))))
          ; Traverse the trie, searching for the longest repeated sequence.
          (multiple-value-bind (local-best-sequence local-sequence-count) (find-longest-repeated-sequence root)
            (when (> (length local-best-sequence) length-longest-sequence)
              (setf length-longest-sequence (length local-best-sequence)
                    longest-sequence local-best-sequence
                    longest-sequence-divisor num
                    longest-sequence-count local-sequence-count)))
          )))
    (list longest-sequence-divisor length-longest-sequence longest-sequence-count longest-sequence))); }}}

(defun populate-trie (digit-list); {{{
  (let (; Note that we because we're looking for a repeated substring,
        ; the max length is half the length of digit-list.
        (max-substring-length (/ (length digit-list) 2))
        (root (make-trie)))

    ; Populate the trie.
    (do ((node root root))        ; We start at the root with each substring,
        ((null digit-list) root)  ; and return it when we run out of substrings.
      (do ((remaining-digits digit-list (rest remaining-digits))
           (num-processed-digits 0 (1+ num-processed-digits)))
          ((or (null remaining-digits)
               (>= num-processed-digits max-substring-length)))
        (let ((current-digit (first remaining-digits)))
          (when (not (aref (trie-children node) current-digit))
            (setf (aref (trie-children node) current-digit) (make-trie)))
          (setf node (aref (trie-children node) current-digit))
          (incf (trie-num node))))
      ; We're finished with this digit.
      (pop digit-list)))); }}}

(defun find-longest-repeated-sequence (root &optional (sequence-count 0)); {{{
  (let ((longest-sequence nil)
        (length-longest-sequence 0)
        (longest-sequence-count sequence-count))
    (loop for i from 0 to (1- (length (trie-children root))) do
      (let ((current-child (aref (trie-children root) i)))
        (when (and current-child
                   (>= (trie-num current-child) (1- longest-sequence-count))
                   (>= (trie-num current-child) 2))
          (multiple-value-bind (current-sequence current-sequence-count) (find-longest-repeated-sequence current-child (trie-num current-child))
            (let ((length-current-sequence (length current-sequence)))
              (when (> (1+ length-current-sequence) length-longest-sequence)
                (setf length-longest-sequence (1+ length-current-sequence)
                      longest-sequence (append (list i) current-sequence)
                      longest-sequence-count current-sequence-count)))))))
    (values longest-sequence longest-sequence-count))
); }}}

(defun rational-to-reversed-list-of-digits (the-rational &optional (max-num-digits 200)); {{{
  (do ((num-digits 0)
       (digits '())
       (remainder the-rational))
      ((or (> num-digits max-num-digits)
           (zerop remainder))
       digits)
    (push (floor (* remainder 10)) digits)
    (incf num-digits)
    (setf remainder (mod (* remainder 10) 1)))); }}}

; Euler published the remarkable quadratic formula:; {{{
;
; n² + n + 41
;
; It turns out that the formula will produce 40 primes for the consecutive
; values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
; divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible
; by 41.
;
; Using computers, the incredible formula  n² - 79n + 1601 was discovered, which
; produces 80 primes for the consecutive values n = 0 to 79. The product of the
; coefficients, 79 and 1601, is 126479.
;
; Considering quadratics of the form:
;
; n² + an + b, where |a|  1000 and |b|  1000
;
; where |n| is the modulus/absolute value of n
; e.g. |11| = 11 and |4| = 4
; Find the product of the coefficients, a and b, for the quadratic expression
; that produces the maximum number of primes for consecutive values of n,
; starting with n = 0.

; First thought: starting with n = 0 means that (n^2 + an + b) simplifies to
; just (b), so any b that isn't prime can be skipped.  Start with
; sieve-of-eratosthenes(1000), and only use primes for b.; }}}

(defparameter primep-cache (make-hash-table))
(defun caching-primep (an-integer); {{{
  (multiple-value-bind (value present)
                       (gethash an-integer primep-cache)
    (if present
      value
      (setf (gethash an-integer primep-cache) (primep an-integer))))); }}}

(defun project-euler-27-1 (); {{{
  (let* ((upper-bound 1000)
         (b-primes (sieve-of-eratosthenes upper-bound))
         (best-a)
         (best-b)
         (best-length 0))
    (loop for b from (- upper-bound) to upper-bound do
      (when (aref b-primes (abs b))
        (loop for a from (- upper-bound) to upper-bound do
          (let ((num-primes 0))
            (do* ((n 0 (1+ n))
                  (result (+ (* n n) (* a n) b)
                          (+ (* n n) (* a n) b)))
                 ((not (primep (abs result))))
              (incf num-primes))
            (when (> num-primes best-length)
              (setf best-a a
                    best-b b
                    best-length num-primes))))))
    (list best-a best-b best-length (* best-a best-b)))); }}}

; Starting with the number 1 and moving to the right in a clockwise direction a; {{{
; 5 by 5 spiral is formed as follows:
;
; 21 22 23 24 25
; 20  7  8  9 10
; 19  6  1  2 11
; 18  5  4  3 12
; 17 16 15 14 13
;
; It can be verified that the sum of the numbers on the diagonals is 101.
;
; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
; formed in the same way?

; 43 44 45 46 47 48 49
; 42 21 22 23 24 25 26
; 41 20  7  8  9 10 27
; 40 19  6  1  2 11 28
; 39 18  5  4  3 12 29
; 38 17 16 15 14 13 30
; 37 36 35 34 33 32 31; }}}

(defun project-euler-28-1 (); {{{
  ;current = current + 10 * increment
  (do ((total 1)
       (current 1)
       (increment 2 (+ 2 increment)))
      ((>= current (* 1001 1001)) total)
    ; Each time you travel round the square, total increases by:
    ;     current +  1 * increment
    ;     current +  2 * increment
    ;     current +  3 * increment
    ;     current +  4 * increment
    ; ---------------------
    ; 4 * current + 10 * increment
    ; current increases by 4 * increment
    (incf total   (+ (* 4 current) (* 10 increment)))
    (incf current (* 4 increment)))); }}}

; Consider all integer combinations of ab for 2  a  5 and 2  b  5:; {{{
;
; 22=4, 23=8, 24=16, 25=32
; 32=9, 33=27, 34=81, 35=243
; 42=16, 43=64, 44=256, 45=1024
; 52=25, 53=125, 54=625, 55=3125
; If they are then placed in numerical order, with any repeats removed, we get
; the following sequence of 15 distinct terms:
;
; 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
;
; How many distinct terms are in the sequence generated by ab for 2  a  100 and
; 2  b  100?; }}}

(defun project-euler-29-1 (); {{{
  (let ((the-hash (make-hash-table)))
    (loop for a from 2 to 100 do
      (loop for b from 2 to 100 do
        (setf (gethash (expt a b) the-hash) t)))
    (hash-table-count the-hash))); }}}

; Surprisingly there are only three numbers that can be written as the sum of; {{{
; fourth powers of their digits:
;
; 1634 = 1^4 + 6^4 + 3^4 + 4^4
; 8208 = 8^4 + 2^4 + 0^4 + 8^4
; 9474 = 9^4 + 4^4 + 7^4 + 4^4
; As 1 = 1^4 is not a sum it is not included.
;
; The sum of these numbers is 1634 + 8208 + 9474 = 19316.
;
; Find the sum of all the numbers that can be written as the sum of fifth powers
; of their digits.; }}}

(defun increment-list-number (the-number); {{{
  (when (and the-number
             (first the-number))
    (setf (first the-number) (mod (1+ (first the-number)) 10))
    (when (zerop (first the-number))
      (increment-list-number (rest the-number))))); }}}

(defun project-euler-30-1 (); {{{
  (let ((the-power 5)
        (max-number-to-check)
        (cache (make-array 10))
        (current-number-as-list)
        (matching-numbers '()))

    ; Figure out the highest number to check:
    ; it's the first value of (* n (expt 9 the-power)) that's less than an
    ; n-digit number whose digits are all 9.
    (setf max-number-to-check
          (do ((all-nines 9 (+ 9 (* all-nines 10)))
               (max-sum (expt 9 the-power) (+ max-sum (expt 9 the-power))))
              ((> all-nines max-sum) max-sum)))
    (loop for i from 0 to the-power do
      (setf current-number-as-list (cons 0 current-number-as-list)))
    (loop for i from 0 to (1- (array-dimension cache 0)) do
      (setf (aref cache i) (expt i the-power)))

    (increment-list-number current-number-as-list)
    (increment-list-number current-number-as-list)
    (loop for current-number from 2 to max-number-to-check do
      (let ((total (reduce #'+
                           (mapcar #'(lambda (x) (aref cache x))
                                   current-number-as-list))))
        (when (= total current-number)
          (push current-number matching-numbers)))
      (increment-list-number current-number-as-list))

    (reduce #'+ matching-numbers))); }}}

; In England the currency is made up of pound, £, and pence, p, and there are; {{{
; eight coins in general circulation:
;
; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;
; It is possible to make £2 in the following way:
;
;     1 x £1 + 1 x 50p + 2 x 20p + 1 x 5p + 1 x 2p + 3 x 1p
;
; How many different ways can £2 be made using any number of coins?; }}}

(defun count-possible-coin-combinations (amount list-of-coins); {{{
  (when (null list-of-coins)
    (error "count-possible-coin-combinations: called with empty coin list"))
  (when (zerop amount)
    (error "count-possible-coin-combinations: called with zero amount"))
  (block nil
    (let ((coin (first list-of-coins)))
      (when (null (rest list-of-coins))
        ; This is the last coin.
        (if (not (zerop (mod amount coin)))
            (return 0)
            (return 1)))
      (let ((num-combinations 0))
        (loop for num-coins from 0 to (floor (/ amount coin)) do
          (let ((remaining-amount (- amount (* coin num-coins))))
            (if (zerop remaining-amount)
                (incf num-combinations)
                (incf num-combinations (count-possible-coin-combinations remaining-amount (rest list-of-coins))))))
        num-combinations)))); }}}

(defun project-euler-31-1 (); {{{
  (count-possible-coin-combinations 200 '(200 100 50 20 10 5 2 1))); }}}

; We shall say that an n-digit number is pandigital if it makes use of all the; {{{
; digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
; through 5 pandigital.
;
; The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing
; multiplicand, multiplier, and product is 1 through 9 pandigital.
;
; Find the sum of all products whose multiplicand/multiplier/product identity
; can be written as a 1 through 9 pandigital.
;
; HINT: Some products can be obtained in more than one way so be sure to only
; include it once in your sum.; }}}

(defun project-euler-32-1 (); {{{
  (let ((results (make-hash-table)))
    (loop for i from 2 to 9999 do
      (loop for j from 2 to i while (<= (* j i) 99999) do
        (let ((product (* i j)))
          (when (is-pandigital (list i j product))
            (setf (gethash product results) t)))))
    (let ((sum 0))
      (maphash #'(lambda (key value)
                         (declare (ignore value))
                         (incf sum key))
               results)
      sum))); }}}

(defun is-pandigital (args); {{{
  (let ((digits-found (make-array 10 :initial-element 0)))
    (dolist (a-number args)
      (do ((digit (mod a-number 10) (mod a-number 10)))
          ((< a-number 1))
        (incf (aref digits-found digit))
        (setf a-number (floor (/ a-number 10)))))
    (let ((result (= 0 (aref digits-found 0))))
      (loop for i from 1 to 9 while result do
        (setf result (= 1 (aref digits-found i))))
      result))); }}}

; The fraction 49/98 is a curious fraction, as an inexperienced mathematician in; {{{
; attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
; correct, is obtained by cancelling the 9s.
;
; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
;
; There are exactly four non-trivial examples of this type of fraction, less
; than one in value, and containing two digits in the numerator and denominator.
;
; If the product of these four fractions is given in its lowest common terms,
; find the value of the denominator.; }}}

(defun project-euler-33-1 (); {{{
  (let ((fractions '()))
    (loop for d1 from 1 to 9 do
      (loop for d2 from 1 to 9 do
        (loop for n1 from 1 to d1 do
          (loop for n2 from 1 to 9
                ; numerator must be less than denominator
                while (or (< n1 d1) (< n2 d2)) do
            (multiple-value-bind (n d)
              (cond
                ((= n1 d1) (values n2 d2))
                ((= n1 d2) (values n2 d1))
                ((= n2 d1) (values n1 d2))
                ((= n2 d2) (values n1 d1)))
              (when (and n
                         (= (/ n d)
                            (/ (+ (* n1 10) n2)
                               (+ (* d1 10) d2))))
                (push (/ n d) fractions)))))))
    (reduce #'* fractions))); }}}

; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.; {{{
;
; Find the sum of all numbers which are equal to the sum of the factorial of
; their digits.
;
; Note: as 1! = 1 and 2! = 2 are not sums they are not included.; }}}

(defun project-euler-34-1 (); {{{
  (let ((curious-numbers '())
        (factorials (make-array 10)))

    (setf (aref factorials 0) 1)
    (loop for i from 1 to 9 do
      (setf (aref factorials i) (* (aref factorials (1- i)) i)))

    ; (* 6 9!) = 2177280
    ; (* 7 9!) = 2540160 - this is our limit
    ; (* 8 9!) = 2903040
    (loop for each-number from 3 to (* 7 (aref factorials 9)) do
      (let ((sum-of-factorials-of-digits 0))
        (do* ((remaining-digits each-number (floor remaining-digits 10))
              (current-digit (mod remaining-digits 10) (mod remaining-digits 10)))
             ((zerop remaining-digits))
          (incf sum-of-factorials-of-digits (aref factorials current-digit)))
        (when (= each-number sum-of-factorials-of-digits)
          (push each-number curious-numbers))))
    (reduce #'+ curious-numbers))); }}}

; The number, 197, is called a circular prime because all rotations of the; {{{
; digits: 197, 971, and 719, are themselves prime.
;
; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
; 73, 79, and 97.
;
; How many circular primes are there below one million?; }}}

(defun number-to-digits (a-number); {{{
  "Convert a number to a list of its digits, in order"
  (let ((digits '()))
    (loop while (> a-number 0) do
      (push (mod a-number 10) digits)
      (setf a-number (floor a-number 10)))
    digits)); }}}

(defun digits-to-number (digits); {{{
  "Convert a list of digits to a number"
  (let ((a-number 0))
    (dolist (digit digits a-number)
      (setf a-number (+ (* a-number 10) digit))))); }}}

(defun is-circular-prime (primes a-number); {{{
  "Check if a-number is a circular prime"
  (let ((digits (number-to-digits a-number))
        (rotated-digits '())
        (is-circular-prime (aref primes a-number)))
    (loop repeat (1- (length digits))
          while is-circular-prime do
      (setf rotated-digits (append rotated-digits (list (pop digits))))
      (let ((possible-prime (digits-to-number (append digits rotated-digits))))
        (setf is-circular-prime (aref primes possible-prime))))
    is-circular-prime)); }}}

(defun project-euler-35-1 (); {{{
  (let ((primes (sieve-of-eratosthenes 1000000))
        (num-circular-primes 0))
    (loop for i from 2 to (1- (array-dimension primes 0)) do
      (when (and (aref primes i)
                 (is-circular-prime primes i))
        (incf num-circular-primes)))
    num-circular-primes)); }}}

; The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.; {{{
;
; Find the sum of all numbers, less than one million, which are palindromic in
; base 10 and base 2.
;
; (Please note that the palindromic number, in either base, may not include
; leading zeros.); }}}

(defun project-euler-36-1 (); {{{
  (let ((palindromes '()))
    (loop for a-number from 1 to 999999 do
      (when (not (zerop (mod a-number 10)))
        (let ((digits (number-to-digits a-number)))
          (when (equal digits (reverse digits))
            (let ((binary-ish (read-from-string (format nil "~B" a-number))))
              (when (not (zerop (mod binary-ish 10)))
                (let ((binary-digits (number-to-digits binary-ish)))
                  (when (equal binary-digits (reverse binary-digits))
                    (push a-number palindromes)))))))))
    (reduce #'+ palindromes))); }}}

; The number 3797 has an interesting property. Being prime itself, it is; {{{
; possible to continuously remove digits from left to right, and remain prime at
; each stage: 3797, 797, 97, and 7. Similarly we can work from right to left:
; 3797, 379, 37, and 3.
;
; Find the sum of the only eleven primes that are both truncatable from left to
; right and right to left.
;
; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.; }}}

(defun is-truncatable-prime-aux (primes digits func); {{{
  (do ((is-truncatable t))
      ((or (not is-truncatable)
           (null digits))
       is-truncatable)
    (let ((a-number (digits-to-number digits)))
      (setf is-truncatable (aref primes a-number)
            digits (funcall func digits))))); }}}

(defun is-truncatable-prime (primes a-number); {{{
  "Determines is a prime is truncatable (PE 37)"
  (let ((digits (number-to-digits a-number)))
    (and (is-truncatable-prime-aux primes digits #'(lambda (digits)
                                                     (rest digits)))
         (is-truncatable-prime-aux primes digits #'(lambda (digits)
                                                     (nreverse digits)
                                                     (pop digits)
                                                     (nreverse digits)))))); }}}

(defun project-euler-37-1 (); {{{
  (let ((truncatable-primes '())
        (num-truncatable-primes 0)
        (primes (sieve-of-eratosthenes 1000000)))
    (loop for a-number from 9 to (1- (array-dimension primes 0)) by 2
          while (< num-truncatable-primes 11) do
      (when (and (aref primes a-number)
                 (is-truncatable-prime primes a-number))
        (push a-number truncatable-primes)
        (incf num-truncatable-primes)))
    (reduce #'+ truncatable-primes))); }}}

; Take the number 192 and multiply it by each of 1, 2, and 3:; {{{
;
; 192 * 1 = 192
; 192 * 2 = 384
; 192 * 3 = 576
; By concatenating each product we get the 1 to 9 pandigital, 192384576. We will
; call 192384576 the concatenated product of 192 and (1,2,3)
;
; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and
; 5, giving the pandigital, 918273645, which is the concatenated product of 9
; and (1,2,3,4,5).
;
; What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
; concatenated product of an integer with (1,2, ... , n) where n > 1?; }}}

(defun project-euler-38-1 (); {{{
  (let ((largest-pandigital 0))
    (loop for a-number from 1 to 10000 do
      (let ((digits '())
            (num-digits 0))
        ; Generate a number containing at least 9 digits
        (loop for multiplier from 1 to 9 
              while (< num-digits 9) do
          (let* ((new-digits (number-to-digits (* a-number multiplier)))
                 (num-new-digits (length new-digits)))
            (setf digits (append digits new-digits)
                  num-digits (+ num-digits num-new-digits))))
        ; Save it if it's pandigital and larger than the current largest number.
        (when (and (= 9 num-digits)
                   (is-pandigital digits))
          (let ((pandigital (digits-to-number digits)))
            (when (< largest-pandigital pandigital)
              (setf largest-pandigital pandigital))))))
    largest-pandigital)); }}}
