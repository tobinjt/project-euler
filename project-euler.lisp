; vim: set foldmethod=marker commentstring=;\ %s :

; strictperl -MList::Util=sum -e 'my @numbers = grep { ($_ % 3 == 0) or ($_ % 5 == 0) } 1 .. 999; print sum(@numbers);'
; 233168
(defun project-euler-1-1 (); {{{
  (let ((total 0))
    (dotimes (current-number 1000 total)
      (cond
        ((zerop (mod current-number 3)) (setf total (+ total current-number)))
        ((zerop (mod current-number 5)) (setf total (+ total current-number))))))); }}}

(defun project-euler-1-2 (); {{{
  (project-euler-1-2-aux '0 '0)); }}}
(defun project-euler-1-2-aux (current-number total); {{{
  (cond
    ((equal current-number 1000) total)
    ((or (zerop (mod current-number 3))
         (zerop (mod current-number 5)))
      (project-euler-1-2-aux (1+ current-number) (+ total current-number)))
    (t (project-euler-1-2-aux (1+ current-number) total)))); }}}

; Create a closure that will return the next Fibonacci number each time its; {{{
; called.  I guessed how to write a closure in LISP; I'd prefer to create an
; anonymous closure at the start of project-euler-2-1, but I haven't figured
; that out yet - I think it's several chapters away in my LISP book.; }}}
(let ((fib-2 0); {{{
      (fib-1 1)
      (fib-count 2))
  (defun fib-next (); {{{
    (let ((fib-current (+ fib-2 fib-1)))
      (setf fib-2 fib-1)
      (setf fib-1 fib-current)
      (setf fib-count (1+ fib-count))
      (print (list fib-count ": " fib-current))
      fib-current)); }}}
); }}}

; I think I could use optional parameters instead of defining an auxilary
; function - I'll try that next time.
(defun project-euler-2-1 (); {{{
  (project-euler-2-1-aux '0)); }}}

(defun project-euler-2-1-aux (total); {{{
  (let ((fib-current (fib-next)))
    (cond
      ((> fib-current 4000000) total)
      ((evenp fib-current)
        (print (list "total: " (+ total fib-current)))
        (project-euler-2-1-aux (+ total fib-current)))
      (t (project-euler-2-1-aux total))))); }}}

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
            t)
          ((zerop (mod an-integer current-factor))
            (return nil))))))); }}}

(defun get-factors (an-integer); {{{
  (do* ((possible-factors (make-sequence-generator 2 (ceiling (sqrt an-integer))))
        (current-factor (funcall possible-factors) (funcall possible-factors))
        (factors nil))
       ((null current-factor) factors)

    (when (zerop (mod an-integer current-factor))
      (setf factors (cons current-factor factors))
      (setf factors (cons (/ an-integer current-factor) factors))))); }}}

(defun project-euler-3-1 (); {{{
  (let ((sorted-factors (sort (get-factors 600851475143) #'>)))
    (dolist (current-factor sorted-factors)
      (when (primep current-factor)
        (return current-factor))))); }}}

(defun make-sequence-generator (start end); {{{
  (let ((previous-number start))
    (lambda ()
      (if (> previous-number end)
        nil
        (let ((result previous-number))
          (setf previous-number (1+ previous-number))
          result))))); }}}

; A palindromic number reads the same both ways. The largest palindrome made; {{{
; from the product of two 2-digit numbers is 9009 = 91 × 99.
;
; Find the largest palindrome made from the product of two 3-digit numbers.; }}}

(defun project-euler-4-1 (); {{{
  (let ((palindrome 0)
        (numbers nil))
    (do ((outer-number 999 (1- outer-number)))
        ((or (< outer-number 100)
            ; Stop when it's not possible to produce a product larger than the
            ; current palindrome.
             (< outer-number (/ palindrome outer-number))))

      (do ((inner-number outer-number (1- inner-number)))
          ((< inner-number 100))

        (let* ((product (* outer-number inner-number))
               (product-string (write-to-string product))
               (reversed-product-string (reverse product-string)))
          (when (and
                  (> product palindrome)
                  (equal product-string reversed-product-string))
            (setf palindrome product)
            (setf numbers (list outer-number inner-number))))))
    (cons palindrome numbers))); }}}

; 2520 is the smallest number that can be divided by each of the numbers from 1; {{{
; to 10 without any remainder.
;
; What is the smallest number that is evenly divisible by all of the numbers
; from 1 to 20?

; Find the prime factors of each number between 1 and 20, including the power,
; so 9 = 3**2.  The smallest number is the product of the highest prime factors.; }}}

(defun project-euler-5-1 (); {{{
  (let* ((highest-number 20)
         ; I don't want to be constantly adding or subtracting 1 to indices.
         (array-size (1+ highest-number))
         ; This breaks the generalisation, but it could easily be replaced if
         ; necessary.
         (primes '(2 3 5 7 11 13 17 19))
         (factors-array (make-array array-size :initial-element 0)))

    (do ((current-number 2 (1+ current-number)))
        ((> current-number highest-number))
      (let ((remainder current-number)
            (current-factors (make-array array-size :initial-element 0)))
        (dolist (current-prime primes)
          (loop
            (when (not (zerop (mod remainder current-prime)))
              (return))
            (setf remainder (/ remainder current-prime))
            (setf (aref current-factors current-prime)
                    (1+ (aref current-factors current-prime)))))
        (dotimes (i highest-number)
          (setf (aref factors-array i) (max (aref current-factors i)
                                            (aref factors-array i))))))
    (let ((result 1))
      (dotimes (i highest-number result)
        (setf result (* result (expt i (aref factors-array i)))))))); }}}

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
  (let* ((n 100)
         (sum-n (/ (* n (1+ n)) 2))
         (square-sum-n (expt sum-n 2))
         ; n(n + 1)(2n + 1)/6
         ; http://en.wikipedia.org/wiki/Sum
         (sum-squares (/ (* n (1+ n) (1+ (* n 2))) 6)))
    (- square-sum-n sum-squares)) ); }}}

; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see; {{{
; that the 6^(th) prime is 13.
;
; What is the 10001^(st) prime number?

; How about: an array of primes, initially containing 2; an infinite loop,
; testing the next number to see if it's a multiple of any of the primes, and if
; not, appending it to the array.  Keep going until the array is full.; }}}

(defun project-euler-7-1 (); {{{
  (let* ((number-of-primes 10001)
         (primes (make-array number-of-primes :initial-element 0))
         (first-prime 2)
         (next-prime-index 1)
         (current-number first-prime))

    (setf (aref primes 0) first-prime)
    (do* ()
         ((equal number-of-primes next-prime-index))

      (let* ((current-index 0)
             (current-prime (aref primes current-index))
             (max-divisor (sqrt current-number)))
        (loop
          (when (zerop current-prime)
            ; shouldn't happen; we ran off the end of the primes array
            (break "current-prime == 0"))
          (when (zerop (mod current-number current-prime))
            ; we found a divisor; current-number is not prime
            (return nil))
          (when (> current-prime max-divisor)
            ; current-number is prime; save it, and move on to the next number
            (setf (aref primes next-prime-index) current-number)
            (setf next-prime-index (1+ next-prime-index))
            (return t))
          (setf current-index (1+ current-index))
          (setf current-prime (aref primes current-index)))

        (setf current-number (1+ current-number))))
    (aref primes (1- number-of-primes)))); }}}

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
  (parse-integer (string (char a-string index)))); }}}

(defun project-euler-8-1 (); {{{
  (let* ((the-number 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
         (the-number-string (write-to-string the-number))
         (number-of-digits 5)
         (digits-last-index (1- number-of-digits))
         (highest-product 1)
         (digits (make-string number-of-digits))
         (highest-digits (make-string number-of-digits)))

    ; populate digits and highest-digits
    (dotimes (i number-of-digits)
      (let ((digit (char the-number-string i)))
        (setf (char digits i) digit)
        (setf (char highest-digits i) digit)
        (setf highest-product (* (get-digit the-number-string i) highest-product))))

    ; loop over the remaining digits in the-number-string
    (do ((i number-of-digits (1+ i)))
        ((equal i (length the-number-string)))

      (let ((product 1))
        ; shift the current group of digits left
        (dotimes (j digits-last-index)
          (setf (char digits j) (char digits (1+ j)))
          (setf product (* product (get-digit digits j))))
        ; append the next digit from the-number-string
        (setf (char digits digits-last-index) (char the-number-string i))
        (setf product (* product (get-digit digits digits-last-index)))
        (when (> product highest-product)
          ; update highest-product and highest-digits
          (setf highest-product product)
          (dotimes (j digits-last-index)
            (setf (char highest-digits j) (char digits j))))) )
    (list highest-product highest-digits))); }}}

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
  (do ((a 1 (1+ a))
       (result nil))
      ((or (not (null result))
           (>= a 500))
        result)

    (do ((b a (1+ b)))
        ((or (not (null result))
             (> b 500))
          result)

      (let* ((sqrt-a2+b2 (sqrt (+ (expt a 2) (expt b 2))))
             (value (+ a b sqrt-a2+b2)))

        (when (equal value 1000)
          (setf result (list a b (- 1000 (+ a b))))
          (setf result
            (list (* a b (first (last result)))
                  result))))))); }}}

; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.; {{{
;
; Find the sum of all the primes below two million.

; This attempt would have taken roughly 40 minutes, with garbage collection
; consuming 60-70% of that time.; }}}

(defun project-euler-10-1 (); {{{
  (do* ((primes '(2))
        (current-number (1+ (first primes)) (+ 2 current-number))
        (sum-of-primes (first primes))
        (sqrt-current-number (sqrt current-number) (sqrt current-number))
        (remaining-primes primes primes))
       ((>= current-number 2000000) sum-of-primes)

    (loop
      (when (zerop (mod current-number (first remaining-primes)))
        ; We found a divisor; current-number is not prime
        (return nil))
      (when (> (first remaining-primes) sqrt-current-number)
        ; We found a prime
        (setf primes (append primes (list current-number)))
        (setf sum-of-primes (+ sum-of-primes current-number))
        (return t))
      (setf remaining-primes (rest remaining-primes))))); }}}

(defun project-euler-10-2 (); {{{
  (let ((primes (sieve-of-eratosthenes 2000000))
        (sum-of-primes 0))
    (dotimes (i (length primes) sum-of-primes)
      (when (is-prime primes i)
        (setf sum-of-primes (+ sum-of-primes i)))))); }}}

(defun is-prime (sieve index); {{{
  (if (< index (array-dimension sieve 0))
    (= 1 (aref sieve index))
    (primep index))); }}}

(defun sieve-of-eratosthenes (upper-bound); {{{
  (let* ((array-size (1+ upper-bound))
         (primes (make-array array-size :initial-element 1 :element-type 'bit)))
    (setf (aref primes 0) 0)
    (setf (aref primes 1) 0)

    (dotimes (i (1+ (ceiling (sqrt array-size))) primes)
      (when (is-prime primes i)
        (do ((index-of-multiples (expt i 2) (+ index-of-multiples i)))
            ((>= index-of-multiples array-size))
          (setf (aref primes index-of-multiples) 0)))))); }}}

(defun seq-list2 (min max); {{{
  (loop for i from min to max collect i)); }}}

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
  (let ((number-string (write-to-string (expt 2 1000)))
        (sum-of-digits 0))

    (dotimes (i (length number-string) sum-of-digits)
      (setf sum-of-digits (+ sum-of-digits
                             (parse-integer (string (char number-string i)))))))); }}}

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
        (incf num-factors 2))))); }}}

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
        (setf primes (append primes (list n-to-divide))))))); }}}

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
    (setf num-factors (* num-factors-n num-factors-n-1)))); }}}

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
          ))) 0 10)); }}}

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
                (setf chain-start val))))))))); }}}

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
        (setf result (/ (* result (- (1+ row) column)) column)))))); }}}

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
      sum-1-1000))); }}}

; n! means n  (n  1)  ...  3  2  1; {{{
;
; Find the sum of the digits in the number 100!; }}}

(defun project-euler-20-1 (); {{{
  (reduce #'+ (mapcar #'(lambda (x) (parse-integer (string x)))
                      (coerce (write-to-string (reduce #'* (loop for i from 1 to 100 collect i))) 'list)))); }}}

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
          (setf result (aref triangle-summed i j))))))); }}}

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
        (incf sunday 7))))); }}}

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
; Find the value of d < 1000 for which 1/d contains the longest recurring cycle
; in its decimal fraction part. ; }}}

(defstruct repeated-sequence; {{{
  (divisor)
  (offset)
  (length)
  (sequence)); }}}

(defun cmp-repeated-sequence (a b); {{{
  "Return true if a should sort before b."
  ; Sort by length (greater is better), falling back to offset (smaller is
  ; better).
  (if (= (repeated-sequence-length a)
          (repeated-sequence-length b))
    (< (repeated-sequence-offset a)
        (repeated-sequence-offset b))
    (> (repeated-sequence-length a)
        (repeated-sequence-length b)))); }}}

(defun project-euler-26-2 (&key (max-n 1000) (max-offset 10) (num-digits 2000)); {{{
  (let ((all-sequences '()))
    (do* ((n 2 (1+ n))
          (new-sequences (find-sequences n :max-offset max-offset :num-digits num-digits)
                         (find-sequences n :max-offset max-offset :num-digits num-digits)))
         ((>= n max-n))
      (sort new-sequences #'cmp-repeated-sequence)
      (when new-sequences
        (setf all-sequences (cons (first new-sequences) all-sequences))))
    (sort all-sequences #'cmp-repeated-sequence)
    (first all-sequences))); }}}

(defun find-sequences (n &key (max-offset 10) (num-digits 2000)); {{{
  "Find repeated sequences in 1/n.  We find the smallest repeated sequence at
   each offset, and return a list of them."
  (let ((sequences '()))
    (let ((reverse-1/n (rational-to-reversed-list-of-digits (/ 1 n) num-digits)))

      ; Strip trailing repeated digits; this greatly speeds up processing
      ; 0.333333... and similar numbers.
      (do ()
          ((or (not (second reverse-1/n))
               (not (= (first reverse-1/n) (second reverse-1/n)))))
        (pop reverse-1/n))

      (let* ((1/n (coerce (reverse reverse-1/n) 'array))
             (half-1/n-length (/ (length 1/n) 2)))
        (do ((offset 0 (1+ offset)))
            ((or (>= offset half-1/n-length)
                 (>= offset max-offset)))
          (block each-sequence
            (do ((sequence-length 1 (1+ sequence-length)))
                ((>= sequence-length half-1/n-length))
              (let ((digits-in-sequence (sequences-are-equal 1/n sequence-length offset)))
                (when digits-in-sequence
                  (push (make-repeated-sequence :divisor n
                                                :offset offset
                                                :length sequence-length
                                                :sequence digits-in-sequence)
                        sequences)
                  (return-from each-sequence)))))))
    sequences))); }}}

(defun sequences-are-equal (the-array sequence-length offset); {{{
  "Checks if two adjacent sequences in the-array, of length sequence-length,
   starting at index offset, are equal.  Returns the repeated sequence if one
   exists, nil otherwise."
  (do ((repeated-sequence (make-array sequence-length))
       (sequence-1-index offset (1+ sequence-1-index))
       (sequence-2-index (+ offset sequence-length) (1+ sequence-2-index))
       (repeated-sequence-index 0 (1+ repeated-sequence-index))
       (sequence-1-end (1- (+ offset sequence-length))))
      ((> sequence-1-index sequence-1-end) repeated-sequence)
    (when (not (array-in-bounds-p the-array sequence-2-index))
      (return nil))
    (when (not (= (aref the-array sequence-1-index)
                  (aref the-array sequence-2-index)))
      (return nil))
    (setf (aref repeated-sequence repeated-sequence-index)
          (aref the-array sequence-1-index)))); }}}

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
                    longest-sequence-count local-sequence-count))))))
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
    (values longest-sequence longest-sequence-count))); }}}

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
      (when (is-prime b-primes (abs b))
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
  "Convert a number to a list of its digits, in order.  Only works for
   positive integers."
  (let ((digits '()))
    (loop while (> a-number 0) do
      (push (mod a-number 10) digits)
      (setf a-number (floor a-number 10)))
    digits)); }}}

; Declare that digits-to-number is a generic function.
(defgeneric digits-to-number (digits))

(defmethod digits-to-number ((digits list)); {{{
  "Convert a list of digits to a number"
  (let ((a-number 0))
    (dolist (digit digits a-number)
      (setf a-number (+ (* a-number 10) digit))))); }}}

(defmethod digits-to-number ((digits array)); {{{
  "Convert an array of digits to a number"
  (let ((a-number 0))
    (dotimes (index (array-dimension digits 0) a-number)
      (setf a-number (+ (* a-number 10) (aref digits index)))))); }}}

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
      (when (and (is-prime primes i)
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
      (setf is-truncatable (is-prime primes a-number)
            digits (funcall func digits))))); }}}

(defun is-truncatable-prime (primes a-number); {{{
  "Determines is a prime is truncatable (PE 37)"
  (let ((digits (number-to-digits a-number)))
    (and (is-truncatable-prime-aux primes digits #'(lambda (digits)
                                                     (rest digits)))
         (is-truncatable-prime-aux primes digits #'(lambda (digits)
                                                     (butlast digits)))))); }}}

(defun project-euler-37-1 (); {{{
  (let ((truncatable-primes '())
        (num-truncatable-primes 0)
        (primes (sieve-of-eratosthenes 1000000)))
    (loop for a-number from 9 to (1- (array-dimension primes 0)) by 2
          while (< num-truncatable-primes 11) do
      (when (and (is-prime primes a-number)
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

; If p is the perimeter of a right angle triangle with integral length sides,; {{{
; {a,b,c}, there are exactly three solutions for p = 120.
;
; {20,48,52}, {24,45,51}, {30,40,50}
;
; For which value of p <= 1000 is the number of solutions maximised?; }}}

(defun project-euler-39-1 (); {{{
  ; a^2 + b^2 = c^2.  We're going to check all combinations of a and b; if c is
  ; integral and a+b+c <= 1000, increment the count for a+b+c.  Finally take the
  ; perimiter with the largest count.
  ; We don't need to check a > 500, or b > 500, because the perimeter would
  ; exceed 1000.
  (let* ((a-and-b-upper-bound 500)
         (perimiter-upper-bound 1000)
         (perimeter-count (make-array (1+ perimiter-upper-bound) :initial-element 0)))

    (dotimes (a a-and-b-upper-bound)
      (unless (zerop a)
        (dotimes (b a)
          (unless (zerop b)
            (let ((c (sqrt (+ (* a a) (* b b)))))
              (when (integerp c)
                (let ((perimeter (+ a b c)))
                  (when (<= perimeter perimiter-upper-bound)
                    (incf (aref perimeter-count perimeter))))))))))

    (let ((perimiter-seen-most-often)
          (max-perimiter-count 0))
      (dotimes (perimeter perimiter-upper-bound perimiter-seen-most-often)
        (when (> (aref perimeter-count perimeter) max-perimiter-count)
          (setf perimiter-seen-most-often perimeter
                max-perimiter-count (aref perimeter-count perimeter))))))); }}}

; An irrational decimal fraction is created by concatenating the positive; {{{
; integers:
;
; 0.12345678910 1 112131415161718192021...
;
; It can be seen that the 12th digit of the fractional part is 1.
;
; If dn represents the nth digit of the fractional part, find the value of the
; following expression.
;
; d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000; }}}

(defun project-euler-40-1 (); {{{
  (let ((product 1))
    (dotimes (index 7 product)
      (setf product (* product (get-nth-digit (expt 10 index))))))); }}}

(defun num-digits-in-n-digit-numbers (n); {{{
  "In the group of n-digit numbers, there are (n * (10^n - 10^(n-1))) digits."
  (* n (- (expt 10 n) (expt 10 (1- n))))); }}}

(defun get-nth-digit (index); {{{
  (let ((n 1)
        ; The indices in the question start at 1, but ours start at zero, so
        ; decrement the requested index.
        (index-remaining (1- index)))
    (do ()
        ((> (num-digits-in-n-digit-numbers n) index-remaining))
      (decf index-remaining (num-digits-in-n-digit-numbers n))
      (incf n))
    ; We now know that the number under index is the
    ;   (floor index-remaining n)'th n-digit number
    (let* ((number-under-index (+ (expt 10 (1- n))
                                  (floor index-remaining n)))
           (digit-within-number-under-index (mod index-remaining n))
           ; Divide number-under-index by divisor to remove the digits following
           ; the digit we want; mod 10 will then strip the digits preceeding the
           ; digit we want.
           (divisor (expt 10 (1- (- n digit-within-number-under-index)))))
      (mod (floor number-under-index divisor) 10)))); }}}

; We shall say that an n-digit number is pandigital if it makes use of all the; {{{
; digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
; also prime.
;
; What is the largest n-digit pandigital prime that exists?; }}}

; The largest pandigital number is 987,654,321.  There are 409,113 permutations; {{{
; of 1 thing, 2 things, . . ., 9 things.  I could use Sieve of Eratosthenes to
; find all primes up to 987,654,321, but that's a lot of memory and calculation;
; instead, I'll generate perumtations in descending order, and test each for
; primeness using trial division.; }}}

(defun project-euler-41-1 (); {{{
  (block pe41
    (do ((n 9 (1- n)))
        ((= n 0))
      (let ((elements (make-array n))
            (sum-of-elements 0))
        (do ((i 0 (1+ i)))
            ((= i n))
          (setf (aref elements i) (- n i))
          (incf sum-of-elements (aref elements i)))
        ; If a number is divisible by 3, the sum of its digits will be divisible
        ; by 3.  The digits don't change between permutations, so if their sum
        ; is divisible by 3, we can skip this group of digits entirely.
        (when (not (zerop (mod sum-of-elements 3)))
          (do ()
              ((not elements))
            (let ((current-number (digits-to-number elements)))
              (when (primep current-number)
                (return-from pe41 current-number)))
            (setf elements (inplace-permute elements #'<)))))))); }}}

(defun factorial (n); {{{
  (reduce #'*
          (loop for i from 1 to n collect i))); }}}

(defun inplace-permute (elements comparator); {{{
  "This generates the next permutation of the array ELEMENTS, modifying it
   in-place.  It returns ELEMENTS if it was permuted, or nil if there are no
   further permutations available.  The COMPARATOR argument determines whether
   the next permutation will ascend (e.g. >) or descend (e.g. <) from the
   current permutation.  It is the caller's responsibility to ensure that the
   array is sorted before this function is first called; also note that the
   first call will return the *second* permutation."
  ; http://en.wikipedia.org/wiki/Permute#Systematic_generation_of_all_permutations
  ;   The following algorithm generates the next permutation lexicographically
  ;   after a given permutation. It changes the given permutation in-place.
  ;   1 Find the largest index k such that a[k] < a[k + 1]. If no such index
  ;     exists, the permutation is the last permutation.
  ;   2 Find the largest index l such that k < l and a[k] < a[l]. Since k + 1 is
  ;     such an index, l is well defined.
  ;   3 Swap a[k] with a[l].
  ;   4 Reverse the sequence from a[k + 1] up to and including the final element
  ;     a[n].

  ; NOTE: tests using the comparator are negated, because we need #'< for
  ; ascending order, but that's counterintuitive for the caller, who'll ask:
  ; "Why do I get the permutation above the current one with less-than, and the
  ; permutation below with greater-than?".
  ; Step 1
  (let ((k))
    (do ((i 0 (1+ i)))
        ((>= i (1- (array-dimension elements 0))))
      (when (not (funcall comparator (aref elements i)
                                     (aref elements (1+ i))))
        (setf k i)))
    (when k
      ; Step 2
      (let ((l (1+ k)))
        (do ((i l (1+ i)))
            ((>= i (array-dimension elements 0)))
          (when (not (funcall comparator (aref elements k)
                                         (aref elements i)))
            (setf l i)))
        ; Step 3
        (rotatef (aref elements k) (aref elements l))
        ; Step 4
        (do ((lower (1+ k) (1+ lower))
             (upper (1- (array-dimension elements 0)) (1- upper)))
            ((>= lower upper))
          (rotatef (aref elements upper) (aref elements lower))))
      elements))); }}}

; The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so; {{{
; the first ten triangle numbers are:
;
; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;
; By converting each letter in a word to a number corresponding to its
; alphabetical position and adding these values we form a word value. For
; example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
; is a triangle number then we shall call the word a triangle word.
;
; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
; containing nearly two-thousand common English words, how many are triangle
; words?; }}}

(defun project-euler-42-1 (); {{{
  "Calculate each word's value and check if it's a triangle number."
  (let ((triangle-numbers (make-hash-table))
        (largest-triangle-number 0)
        (largest-triangle-number-n 0)
        (num-triangle-words 0))
    (dolist (word (read-comma-delimited-file #p"words.txt") num-triangle-words)
      (let ((sum (sum-alphabetic-positions word)))
        ; Calculate triangle numbers until one exceeds sum.
        (do ()
            ((>= largest-triangle-number sum))
          (incf largest-triangle-number-n)
          (setf largest-triangle-number (calculate-triangle-number largest-triangle-number-n))
          (setf (gethash largest-triangle-number triangle-numbers) t))
        (when (gethash sum triangle-numbers)
          (incf num-triangle-words)))))); }}}

(defun letter-to-alphabetical-position (letter); {{{
  "Return the alphabetical position of a letter, or nil if the letter is not
   found."
  (let ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (uc-letter (char-upcase letter)))
    (dotimes (i (array-dimension letters 0))
      (when (char-equal uc-letter (aref letters i))
        (return (1+ i)))))); }}}

(defun alphabetical-position-to-letter (a-number); {{{
  "Return the letter at that alphabetical position, or nil if the position is
   invalid."
  (let ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (index (1- a-number)))
    (when (array-in-bounds-p letters index)
      (aref letters index)))); }}}

(defun sum-alphabetic-positions (a-string); {{{
  "Returns the sum of the alphabetical positions of the letters in the string
   argument."
  (reduce #'+
          (map 'list
                #'letter-to-alphabetical-position
                a-string))); }}}

(defun calculate-triangle-number (a-number); {{{
  "Calculates Triangle Number N."
  (/ (* a-number (1+ a-number)) 2)); }}}

(defun read-comma-delimited-file (filename); {{{
  "Reads a comma-separated list of strings (double quotes required) from
   FILENAME, returning a list of strings."
  (let ((words (mapcan #'(lambda (x) (nsubstitute #\  #\, x))
                       (read-whole-file filename))))
    (read-from-string (concatenate 'string "(" words ")" )))); }}}

(defun read-whole-file (filename); {{{
  "Reads all of filename with #'read-line, returning a list of lines."
  (with-open-file (file filename)
    (do ((lines '())
         (line (read-line file nil nil)
               (read-line file nil nil)))
        ((not line) (nreverse lines))
      (push line lines)))); }}}

; The number, 1406357289, is a 0 to 9 pandigital number because it is made up of; {{{
; each of the digits 0 to 9 in some order, but it also has a rather interesting
; sub-string divisibility property.
;
; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
; the following:
;
; d2d3d4=406 is divisible by 2
; d3d4d5=063 is divisible by 3
; d4d5d6=635 is divisible by 5
; d5d6d7=357 is divisible by 7
; d6d7d8=572 is divisible by 11
; d7d8d9=728 is divisible by 13
; d8d9d10=289 is divisible by 17
; Find the sum of all 0 to 9 pandigital numbers with this property.; }}}

(defun project-euler-43-1 (); {{{
  (let ((divisors #(2 3 5 7 11 13 17))
        (results '())
        (elements #(0 1 2 3 4 5 6 7 8 9)))

    ; Loop until we run out of permutations
    (do ()
        ((not elements))
      (let ((divisible t))
        ; Loop until one of the pieces is not a multiple of a divisor, or we
        ; run out of divisors.
        (do ((i 0 (1+ i)))
            ((or (not divisible)
                 (= i (array-dimension divisors 0))))
          ; Calculate this piece
          (let ((piece (+ (* (aref elements (+ i 1)) 100)
                          (* (aref elements (+ i 2)) 10)
                             (aref elements (+ i 3)))))
            ; Is it a multiple of the divisor?
            (setf divisible (zerop (mod piece (aref divisors i))))))
        (when divisible
          (push (digits-to-number elements) results))
        (setf elements (inplace-permute elements #'>))))
    (reduce #'+ results))); }}}

; Pentagonal numbers are generated by the formula, Pn=n(3n - 1)/2. The first ten; {{{
; pentagonal numbers are:
;
; 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
;
; It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70
; - 22 = 48, is not pentagonal.
;
; Find the pair of pentagonal numbers, Pj and Pk, for which their sum and
; difference is pentagonal and D = |Pk - Pj| is minimised; what is the value of
; D?; }}}

(defun project-euler-44-1 (); {{{
  (do ((best-pair)
       (best-difference)
       (numbers (list (pentagonal-number 1)) (cons current numbers))
       (current (pentagonal-number 2) (pentagonal-number n))
       (n 3 (1+ n)))
      ((and best-pair
            ; We've found the result when the difference between the last two
            ; pentagonal numbers is greater than the difference between the
            ; best-pair of pentagonal numbers.  I know that the first two
            ; numbers aren't a solution, so I'm not checking that numbers has at
            ; least two elements.
            (>= (- (first numbers) (second numbers))
                best-difference))
       best-difference)
    ; Check each pairing of the current pentagonal number and the previous
    ; pentagonal numbers.
    (dolist (a-number numbers)
      (let ((sum (+ current a-number))
            (difference (- current a-number)))
        ; When we reach a pair whose difference is greater than the best pair's
        ; difference, terminate the loop.
        (when (and best-pair
                   (> difference best-difference))
          (return nil))
        (when (and (pentagonal-number-p sum)
                   (pentagonal-number-p difference))
          ; Is this pair better than the best pair?
          (when (or (null best-pair)
                    (< difference best-difference))
            (setf best-pair (list current a-number)
                  best-difference difference))))))); }}}

(defun pentagonal-number (n); {{{
  "Returns the nth pentagonal number."
  ; Pn=n(3n - 1)/2
  (/ (* n (1- (* 3 n))) 2)); }}}

(defun pentagonal-number-p (a-number); {{{
  "Tests if p is pentagonal.  If p is pentagonal, returns n, where
   p is (pentagonal-number n).  Returns nil if p is not pentagonal."
  ; n=(sqrt(1 + 24x) + 1)/6
  (let ((n (/ (1+ (sqrt (1+ (* 24 a-number)))) 6)))
    (when (integerp n)
      n))); }}}

; Triangle, pentagonal, and hexagonal numbers are generated by the following; {{{
; formulae:
;
; Triangle    Tn=n(n+1)/2    1, 3, 6, 10, 15, ...
; Pentagonal  Pn=n(3n-1)/2   1, 5, 12, 22, 35, ...
; Hexagonal   Hn=n(2n-1)     1, 6, 15, 28, 45, ...
; It can be verified that T285 = P165 = H143 = 40755.
;
; Find the next triangle number that is also pentagonal and hexagonal.; }}}

(defun project-euler-45-1 (); {{{
  ; Wikipedia tells us that all hexagonal nubers are triangle numbers, so we're
  ; looking for the first hexagonal number after H143 that's pentagonal.
  (do ((n 144 (1+ n)))
      ((pentagonal-number-p (hexagonal-number n)) (hexagonal-number n)))); }}}

(defun triangle-number (a-number); {{{
  "Calculates Triangle Number N."
  (/ (* a-number (1+ a-number)) 2)); }}}

(defun triangle-number-p (a-number); {{{
  "Tests if t is a triangle number.  If t is a triangle number, returns n,
   where t is (triangle-number n).  Returns nil if t is not a triangle number."
  ; n = (-1 +/- (sqrt ((* 8 a-number) + 1))) / 2
  ; If n is an integer, then:
  ;    2n is even
  ;    2n - 1 is odd
  ; So, if (sqrt (8x + 1)) is odd, x is a triangle number

  ; This doesn't work for negative numbers.
  (when (plusp a-number)
    (let ((root (sqrt (1+ (* 8 a-number)))))
      (when (oddp root))
        (/ (1- root) 2)))); }}}

(defun hexagonal-number (a-number); {{{
  "Calculates Hexagonal Number N."
  (* a-number (1- (* a-number 2)))); }}}

(defun hexagonal-number-p (a-number); {{{
  "Tests if t is hexagonal.  If t is hexagonal, returns n, where
   t is (hexagonal-number n).  Returns nil if t is not hexagonal."
   ; n = (sqrt (8x + 1) + 1)/4
   (let ((n (/ (1+ (sqrt (1+ (* 8 a-number)))) 4)))
     (when (integerp n)
       n))); }}}

; It was proposed by Christian Goldbach that every odd composite number can be; {{{
; written as the sum of a prime and twice a square.
;
; 9 = 7 + (2 * 1^2)
; 15 = 7 + (2 * 2^2)
; 21 = 3 + (2 * 3^2)
; 25 = 7 + (2 * 3^2)
; 27 = 19 + (2 * 2^2)
; 33 = 31 + (2 * 1^2)
; It turns out that the conjecture was false.
;
; What is the smallest odd composite that cannot be written as the sum of a
; prime and twice a square?; }}}

; primes is an array returned by sieve-of-eratosthenes; {{{
; doubled-squares is a hash table containing doubled squares
; returns t or nil; }}}

(defun is-goldback-number (primes doubled-squares the-number); {{{
  (do ((i 1 (+ 2 i)))
      ((>= i the-number) nil)
    (when (is-prime primes i)
      (let ((remainder (- the-number i)))
        (when (gethash remainder doubled-squares)
          (return t)))))); }}}

; 1 Generate a large number of primes; {{{
; 2 Generate a large number of (2 * n^2)
; 3 Iterate over non-prime odd numbers, checking if (odd-number - prime) is one
; of the doubled squares; the first one that isn't is the first non-Goldbach
; number.; }}}

(defun project-euler-46-1 (); {{{
  (let ((primes (sieve-of-eratosthenes 100))
        (found-primes-less-than 100)
        (doubled-squares (make-hash-table))
        (seed-for-doubled-squares 0)
        (max-doubled-square 0))
    (do ((n 9 (+ n 2)))
        (nil)
      (do ()
          ((< n max-doubled-square))
        (incf seed-for-doubled-squares)
        (setf max-doubled-square (* 2 (expt seed-for-doubled-squares 2)))
        (setf (gethash max-doubled-square doubled-squares) 1))
      (when (> n found-primes-less-than)
        (setf found-primes-less-than (* 2 found-primes-less-than))
        (setf primes (sieve-of-eratosthenes found-primes-less-than)))
      (when (and (not (is-prime primes n))
                 (not (is-goldback-number primes doubled-squares n)))
        (format t "~A~%" n)
        (return))))); }}}

; The first two consecutive numbers to have two distinct prime factors are:; {{{
;
; 14 = 2 * 7
; 15 = 3 * 5
;
; The first three consecutive numbers to have three distinct prime factors are:
;
; 644 = 2² * 7 * 23
; 645 = 3 * 5 * 43
; 646 = 2 * 17 * 19.
;
; Find the first four consecutive integers to have four distinct primes factors.
; What is the first of these numbers?; }}}

; Returns a list of prime factors of the-number; this list can contain; {{{
; duplicates.  sieve should be an array returned by sieve-of-eratosthenes, and
; must contain at least the-number elements to factor primes correctly.; }}}

(defun prime-factors (sieve the-number); {{{
  (when (> the-number (1- (array-dimension sieve 0)))
    (error "prime-factors: sieve too small: ~A > ~A"
           the-number (1- (array-dimension sieve 0))))

  (let ((factors '())
        (remainder the-number))
    (dotimes (i (array-dimension sieve 0))
      (when (= remainder 1)
        (return))
      (when (is-prime sieve i)
        (do ()
            ((not (zerop (mod remainder i))))
          (push i factors)
          (setf remainder (/ remainder i)))))
    (nreverse factors))); }}}

(defun project-euler-47-1 (); {{{
  (let* ((sieve-size 10000)
         (sieve (sieve-of-eratosthenes sieve-size))
         (num-numbers-found 0)
         (first-number-found))
    (do* ((i (* 2 3 5 7) (1+ i))
          (factors (remove-duplicates (prime-factors sieve i))
                   (remove-duplicates (prime-factors sieve i))))
         ((= 4 num-numbers-found) first-number-found)
      (if (= 4 (length factors))
        (progn
          (when (= 0 num-numbers-found)
            (setf first-number-found i))
          (incf num-numbers-found))
        (setf num-numbers-found 0))
      ; Extend the sieve when i is nearly twice its size.
      (when (> i (- sieve-size 10))
        (setf sieve-size (* sieve-size 2))
        (setf sieve (sieve-of-eratosthenes sieve-size)))))); }}}

; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.; {{{
;
; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.; }}}

(defun project-euler-48-1 (); {{{
  (let ((sum 0))
    (do ((i 1 (1+ i)))
        ((> i 1000) (mod sum 10000000000))
      (setf sum (+ sum (expt i i)))))); }}}

; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms; {{{
; increases by 3330, is unusual in two ways: (i) each of the three terms are
; prime, and, (ii) each of the 4-digit numbers are permutations of one another.
;
; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
; exhibiting this property, but there is one other 4-digit increasing sequence.
;
; What 12-digit number do you form by concatenating the three terms in this
; sequence?; }}}

; doarrayi and doarray taken from; {{{
; http://unbox.org/wisp/var/bryan/lib/tricks/array.lisp; }}}

(defmacro doarrayi ((one n array &optional out) &body body); {{{
  (let ((i (gensym)))
    `(let ((,i (length ,array)))
       (dotimes (,n ,i ,out)
         (let ((,one (aref ,array ,n)))
           ,@body))))); }}}

(defmacro doarray ((one array &optional out) &body body); {{{
  (let ((j (gensym)))
    `(doarrayi (,one ,j ,array ,out) ,@body))); }}}

(defun make-n-element-lists (n the-list); {{{
  (let ((num-elements (length the-list)))
    (cond
      ((> n num-elements)
        (error "make-n-element-lists: too few elements in list"))
      ((= n num-elements)
        (list the-list))
      (t
        (cons (subseq the-list 0 n) (make-n-element-lists n (rest the-list))))))); }}}

(defun powerset (the-set); {{{
  (if the-set
    (let ((powersubset (powerset (rest the-set))))
      (append powersubset (mapcar #'(lambda (subset)
                                      (cons (first the-set) subset))
                                  powersubset)))
    (list '()) )); }}}

(defun project-euler-49-1 (); {{{
  (let ((primes (sieve-of-eratosthenes 10000))
        (permutations (make-hash-table :TEST 'EQUAL)))

    ; Place primes into buckets where every number in a bucket is composed from
    ; the same digits.
    (doarrayi (ignored i primes permutations)
      (declare (ignore ignored))
      (when (and (is-prime primes i)
                 (>= i 1000))
        (let ((sorted-digits (write-to-string (sort (number-to-digits i) #'<))))
          (setf (gethash sorted-digits permutations)
                (cons i (gethash sorted-digits permutations))))))

    ; Some buckets will have more than three primes, and any three of those
    ; primes could be the trio we're searching for, so we construct the powerset
    ; of every bucket with more than three primes.
    (let ((list-of-lists-of-possible-answers '()))
      (maphash #'(lambda (_ bucket)
                   (declare (ignore _))
                   (if (< 3 (length bucket))
                     (push (powerset bucket) list-of-lists-of-possible-answers)
                     (push (list bucket) list-of-lists-of-possible-answers)))
               permutations)

      ; Finally, filter out anything not matching the criteria.
      (remove-if #'null
                (mapcar #'(lambda (list-of-possible-answers)
                            (remove-if-not #'(lambda (possible-answer)
                                               (and (= 3 (length possible-answer))
                                                    (= (- (first possible-answer) (second possible-answer))
                                                       (- (second possible-answer) (third possible-answer)))))
                                           list-of-possible-answers))
                        list-of-lists-of-possible-answers))))); }}}

; The prime 41, can be written as the sum of six consecutive primes:; {{{
;
; 41 = 2 + 3 + 5 + 7 + 11 + 13
; This is the longest sum of consecutive primes that adds to a prime below
; one-hundred.
;
; The longest sum of consecutive primes below one-thousand that adds to a prime,
; contains 21 terms, and is equal to 953.
;
; Which prime, below one-million, can be written as the sum of the most
; consecutive primes?; }}}

(defun project-euler-50-1 (); {{{
  (let* ((upper-limit 1000000)
         (prime-array (sieve-of-eratosthenes upper-limit))
         (prime-list '())
         (max-primes-to-sum 0))
    ; Create a list of all primes less than upper-limit/2.
    (doarrayi (ignored i prime-array)
      (declare (ignore ignored))
      (when (is-prime prime-array i)
        (push i prime-list)
        ; Discard anything > upper-limit/2; adding two of anything >
        ; upper-limit/2 is guaranteed to be > upper-limit.
        (when (> i (/ upper-limit 2))
          (return))))
    (setf prime-list (nreverse prime-list))

    ; Figure out the maximum number of primes to sum.
    (do* ((pointer-to-prime prime-list (rest pointer-to-prime))
          (sum 0 (incf sum (first pointer-to-prime))))
         ((or (not pointer-to-prime)
             (> sum upper-limit)))
      (incf max-primes-to-sum))

    ; Sum every sequence of N primes, max-primes-to-sum >= N >= 2, skipping
    ; sequences that come after a sequence whose sum is >= upper-limit.
    (do ((num-primes-in-sum max-primes-to-sum (decf num-primes-in-sum))
         (pointer-to-prime-to-subtract prime-list prime-list)
         (pointer-to-prime-to-add prime-list prime-list)
         (sum-of-primes 0 0)
         (answer nil))
        ((or answer
             (< num-primes-in-sum 2)) answer)

      ; Create the sliding window.
      (dotimes (i num-primes-in-sum)
        (incf sum-of-primes (first pointer-to-prime-to-add))
        (setf pointer-to-prime-to-add (rest pointer-to-prime-to-add)))

      ; Check the first possible answer.
      (when (and (< sum-of-primes upper-limit)
                 (is-prime prime-array sum-of-primes))
        (setf answer sum-of-primes))

      ; Slide the window.
      (do ()
          ((or (not pointer-to-prime-to-add)
               (> sum-of-primes upper-limit)))
        (when (is-prime prime-array sum-of-primes)
          (setf answer sum-of-primes))
        (incf sum-of-primes (first pointer-to-prime-to-add))
        (decf sum-of-primes (first pointer-to-prime-to-subtract))
        (setf pointer-to-prime-to-add (rest pointer-to-prime-to-add)
              pointer-to-prime-to-subtract (rest pointer-to-prime-to-subtract)))))); }}}

; By replacing the 1st digit of *3, it turns out that six of the nine possible; {{{
; values: 13, 23, 43, 53, 73, and 83, are all prime.
;
; By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
; number is the first example having seven primes among the ten generated
; numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and
; 56993. Consequently 56003, being the first member of this family, is the
; smallest prime with this property.
;
; Find the smallest prime which, by replacing part of the number (not
; necessarily adjacent digits) with the same digit, is part of an eight prime
; value family.; }}}

(defun get-digit-positions (a-number); {{{
  "Returns an array of 10 lists, each list containing the indices where digit X
   can be found.  E.g.
      (get-digit-positions 1123)
   returns
      #(NIL (0 1) (2) (3) NIL NIL NIL NIL NIL NIL)"
  (let ((result (make-array 10)))
    (doarrayi (digit i (format nil "~A" a-number))
      (let ((index (parse-integer (string digit))))
        (setf (aref result index) (cons i (aref result index)))))
    (doarrayi (a-list i result result)
      (setf (aref result i) (nreverse a-list))))); }}}

(defun return-prime-family-candidates (prime); {{{
  "Substitute all repeated digits in prime with '*', returning a list of
   generated strings, e.g
      (return-prime-family-candidates 56113)
   returns
      (\"5*113\" \"*6113\" \"5611*\" \"56**3\" \"56*13\" \"561*3\")"
  (let* ((positions (get-digit-positions prime))
         (position-powersets (map 'list
                                  #'powerset
                                  positions))
         (prime-string (format nil "~A" prime))
         (generated-strings '()))
    ; For each digit:
    (dolist (powerset position-powersets)
      ; For each set of digit positions:
      (dolist (digit-positions powerset)
        ; Skip the empty set
        (when digit-positions
          (let ((new-string (copy-seq prime-string)))
            (dolist (i digit-positions)
              (setf (char new-string i) #\*))
            (push new-string generated-strings)))))
    generated-strings)); }}}

(defun project-euler-51-1 (&optional (upper-limit 1000000)
                                     (num-primes-needed 8)); {{{
  (let ((primes (sieve-of-eratosthenes upper-limit))
        (counters (make-hash-table :test 'equal))
        (first-primes (make-hash-table :test 'equal)))

    (block check-each-prime
      (doarrayi (ignored prime primes)
        (declare (ignore ignored))
        (when (is-prime primes prime)
          (dolist (string-pattern (return-prime-family-candidates prime))
            (incf (gethash string-pattern counters 0))
            (when (not (gethash string-pattern first-primes))
              (setf (gethash string-pattern first-primes) prime))
            (when (>= (gethash string-pattern counters)
                      num-primes-needed)
              (return-from check-each-prime
                           (gethash string-pattern first-primes))))))))); }}}

; It can be seen that the number, 125874, and its double, 251748, contain; {{{
; exactly the same digits, but in a different order.
;
; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
; contain the same digits.; }}}

(defun project-euler-52-1 (); {{{
  (block found-answer
    ; Only consider numbers between 10^n and ceil((10^(n+1))/6); for any x
    ; outside those ranges, 6x will have an extra digit.
    (do* ((start 10 (* start 10))
          (end (ceiling (/ (* 10 start) 6))
               (ceiling (/ (* 10 start) 6))))
         (nil)
      (do* ((x start (1+ x))
            (sorted-digits (sort (number-to-digits x) #'<)
                           (sort (number-to-digits x) #'<)))
           ((> x end))
        (block list-of-multipliers
          (dolist (multiplier '(2 3 4 5 6))
            (when (not (equal sorted-digits
                              (sort (number-to-digits (* x multiplier)) #'<)))
              (return-from list-of-multipliers nil)))
          (return-from found-answer x)))))); }}}

; Loop from start to end *inclusive*.; {{{
; This is far more complex than it needs to be, and the reason is SBCL whining
; about deleting unused code.  Without the extra logic, if the start and end
; arguments are known at compile time, SBCL will whine that it's deleting unused
; code - in fact, it whines so much about this usage that it drowns out
; legitimate warnings.  So, we specially handle the case where start and end
; pass numberp at compile time.  We still get warnings about unreachable code,
; but but I think that's in cases like:
;   (dofromto (0 (length an-array) i)
; where SBCL's type inference tells it that (length an-array) can't be negative.
; I;m not going to worry about that now.; }}}

(defmacro dofromto ((start end counter &optional result) &body body); {{{
  (let ((_start (gensym))
        (_end (gensym))
        (_increment (gensym))
        (_comparitor (gensym)))
    `(let ((,_start ,start)
           (,_end ,end)
           (,_increment)
           (,_comparitor))
       ,(if (and (numberp start)
                 (numberp end))
          (if (< start end)
            `(setf ,_increment #'1+
                   ,_comparitor #'>)
            `(setf ,_increment #'1-
                   ,_comparitor #'<))
          `(if (< ,_start ,_end)
             (setf ,_increment #'1+
                   ,_comparitor #'>)
             (setf ,_increment #'1-
                   ,_comparitor #'<)))
        (do ((,counter ,_start (funcall ,_increment ,counter)))
            ((funcall ,_comparitor ,counter ,_end) ,result)
          ,@body)))); }}}

; There are exactly ten ways of selecting three from five, 12345:; {{{
;
; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
;
; In combinatorics, we use the notation, 5C3 = 10.
;
; In general,
;
; nCr = n!/(r!(n-r)!)
;
; where r <= n, n! = n*(n-1)...*3*2*1, and 0! = 1.
;
; It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
;
; How many, not necessarily distinct, values of nCr, for 1 <= n <= 100, are
; greater than one-million?; }}}

; Thought 2: use Pascal's triangle instead.

(defun project-euler-53-1 (); {{{
  (let* ((max-num-values 100)
         (threshold 1000000)
         (num-combinations '((1))))
    ; First generate the number of combinations, using Pascal's Triangle.
    (dotimes (unused max-num-values)
      (setf num-combinations
            (append num-combinations
                    (list (mapcar #'+
                                  (cons 0 (first (last num-combinations)))
                                  (append (first (last num-combinations)) '(0)))))))
    ; Now count the numbers greater than 1000000.
    (reduce #'+
            (mapcar #'(lambda (a-list)
                        (count-if #'(lambda (num)
                                           (> num threshold))
                                       a-list))
                    num-combinations)))); }}}

; In the card game poker, a hand consists of five cards and are ranked, ; {{{
; from lowest to highest, in the following way:
;
; High Card: Highest value card.
; One Pair: Two cards of the same value.
; Two Pairs: Two different pairs.
; Three of a Kind: Three cards of the same value.
; Straight: All cards are consecutive values.
; Flush: All cards of the same suit.
; Full House: Three of a kind and a pair.
; Four of a Kind: Four cards of the same value.
; Straight Flush: All cards are consecutive values of same suit.
; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
; The cards are valued in the order:
; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
;
; If two players have the same ranked hands then the rank made up of the highest
; value wins; for example, a pair of eights beats a pair of fives (see example 1
; below). But if two ranks tie, for example, both players have a pair of queens,
; then highest cards in each hand are compared (see example 4 below); if the
; highest cards tie then the next highest cards are compared, and so on.
;
; Consider the following five hands dealt to two players:
; <snipped>
; The file, poker.txt, contains one-thousand random hands dealt to two players.
; Each line of the file contains ten cards (separated by a single space): the
; first five are Player 1's cards and the last five are Player 2's cards. You
; can assume that all hands are valid (no invalid characters or repeated cards),
; each player's hand is in no specific order, and in each hand there is a clear
; winner.
;
; How many hands does Player 1 win?; }}}

(defstruct card; {{{
  (value)
  (suit)); }}}

(defstruct hand; {{{
  "All cards attributes are either nil or an array of cards, sorted in
   descending order by value (suit is ignored)."
  (string-form)
  (cards)
  (scoring-cards)
  (non-scoring-cards)
  (rank)); }}}

(defun parse-card-value (value); {{{
  "Convert face cards to a numeric value; return other values unchanged."
  (case value
    (#\T 10)
    (#\J 11)
    (#\Q 12)
    (#\K 13)
    (#\A 14)
    (otherwise (parse-integer (string value))))); }}}

(defun un-parse-card-value (value); {{{
  "Convert a numeric card value to a string."
  (case value
    (10 "T")
    (11 "J")
    (12 "Q")
    (13 "K")
    (14 "A")
    (otherwise (format nil "~A" value)))); }}}

(defun sort-cards (cards); {{{
  "Sort cards in-place in descending order by value (suit is ignored)."
  (sort cards #'> :key #'card-value)); }}}

(defun parse-hand (line); {{{
  " Parse '8C TS KC 9H 4S', returning a hand."
  (let ((cards (make-array 5))
        (hand))
    (dofromto (0 4 card-number)
      (let* ((offset (* card-number 3))
             (value (aref line offset))
             (suit (aref line (1+ offset))))
          (setf (aref cards card-number)
                (make-card :value (parse-card-value value)
                           :suit suit))))
    (sort-cards cards)
    (setf hand (make-hand :cards cards))
    (setf (hand-string-form hand) (make-hand-string-form hand))
    hand)); }}}

(defun read-hands-file (hands-fh); {{{
  "Read a file (typically poker.txt), returning a list of lists of hands."
  (let ((hands '()))
    (do ((line (read-line hands-fh nil) (read-line hands-fh nil)))
        ((null line) hands)
      (push (list (parse-hand (subseq line 0 14))
                  (parse-hand (subseq line 15)))
            hands))
    (nreverse hands))); }}}

(defun make-hand-string-form (hand); {{{
  "Returns a human-readable representation of the hand."
  (let ((components '()))
    (loop-over-hand hand
                    #'(lambda (ignored-card card)
                        (declare (ignore ignored-card))
                        (push (un-parse-card-value (card-value card))
                              components)
                        (push (string (card-suit card)) components)
                        (push " " components)))
    (pop components)
    (format nil "~{~A~}" (nreverse components)))); }}}

(defun loop-over-hand (hand func); {{{
  "Iterate over the cards in hand, calling (func first_card current_card) for
   each one.  Returns true if func returns true for all five cards."
  (let* ((result t)
         (cards (hand-cards hand))
         (first-card (aref cards 0)))
    (dofromto (0 4 i result)
      (setf result (and (funcall func first-card (aref cards i))
                        result))))); }}}

(defun hand-same-suit (hand); {{{
  "Return true if all cards in the hand have the same suit, false otherwise."
  (loop-over-hand hand #'(lambda (card-1 card-2)
                           (char-equal (card-suit card-1)
                                       (card-suit card-2))))); }}}

(defun hand-descending-values (hand); {{{
  "Return true if the cards in the hand are in descending order, false
   otherwise."
  (let ((comparison-value (hand-card-value hand 0)))
    (loop-over-hand hand #'(lambda (card-1 card-2)
                             (declare (ignore card-1))
                             (when (= (card-value card-2)
                                      comparison-value)
                               (setf comparison-value
                                     (1- comparison-value))))))); }}}

(defun hand-card-value (hand i); {{{
  "Return the value of the card at index i of the hand."
  (card-value (aref (hand-cards hand) i))); }}}

(defun group-cards-by-value (hand); {{{
  "Return a hash table mapping card-value -> list-of-cards."
  (let ((value-map (make-hash-table)))
    (loop-over-hand hand
                    #'(lambda (ignored-card card)
                        (declare (ignore ignored-card))
                        (let ((value (card-value card)))
                          (setf (gethash value value-map)
                                (push card (gethash value value-map '()))))))
    value-map)); }}}

(defun count-items-in-hash (a-hash); {{{
  "Returns a hash containing (key => (length value)) for every
   (key => value) pair in a-hash."
  (let ((result (make-hash-table)))
    (maphash #'(lambda (key value)
                 (setf (gethash key result) (length value)))
             a-hash)
    result)); }}}

(defun set-scoring-cards-from-hash (hand cards-by-value
                                    num-cards-by-value scoring-count); {{{
  "Set scoring-cards and non-scoring-cards.  When n is 2 and
   there are two pairs in the hand, scoring-cards will contain all 4 cards."
  (let ((scoring-cards '())
        (non-scoring-cards '()))

    (maphash #'(lambda (key value)
                 ; I feel like I should be able to write this more concisely,
                 ; but my attempts with push, nconc, and append were either
                 ; non-functional or just as long.
                 (if (equal value scoring-count)
                   (setf scoring-cards
                         (concatenate 'list scoring-cards
                                            (gethash key cards-by-value)))
                   (setf non-scoring-cards
                         (concatenate 'list non-scoring-cards
                                            (gethash key cards-by-value)))))
             num-cards-by-value)

    (setf (hand-scoring-cards hand)
          (sort-cards (coerce scoring-cards 'array)))
    (setf (hand-non-scoring-cards hand)
          (sort-cards (coerce non-scoring-cards 'array))))); }}}

(defun has-n-cards-with-same-value (hand n &optional (set-scoring-cards t)); {{{
  "Returns true if there are n cards with the same value.  Also modifies cards,
   setting scoring-cards and non-scoring-cards appropriately."
  (let* ((cards-by-value (group-cards-by-value hand))
         (num-cards-by-value (count-items-in-hash cards-by-value))
         (result nil))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (when (= value n)
                   (setf result t)
                   (when set-scoring-cards
                     (set-scoring-cards-from-hash hand cards-by-value
                                                  num-cards-by-value value))))
             num-cards-by-value)
    result)); }}}

(defun test-one-function (func good-hands bad-hands); {{{
  (format t "Testing ~A" func)
  (let ((result t)
        (tests (list (list func good-hands)
                     (list #'(lambda (hand)
                               (not (funcall func hand)))
                           bad-hands))))

    (dolist (test-pair tests)
      (dolist (hand-string (second test-pair))
        (if (funcall (first test-pair) (parse-hand hand-string))
          (format t " t")
          (progn
            (format t " NIL")
            (setf result nil)))))

    (format t "~%")
    result)); }}}

(defun test-is-foo-functions (); {{{
  (and
    (test-one-function #'is-royal-flush     '("AS TS KS JS QS" "TC JC QC KC AC")
                                            '("TC JC QC KC AS" "TC JC QC KC 4C"))
    (test-one-function #'is-straight-flush  '("5S 8S 6S 7S 9S" "TC JC QC KC AC")
                                            '("TC JC QC KC AS" "2S 4D TC 6S 8H"))
    (test-one-function #'is-four-of-a-kind  '("2S 2C 2D 4D 2H" "3D TD TC TS TH")
                                            '("2S 2C 3D 4D 2H" "3D TD 6C TS TH"))
    (test-one-function #'is-full-house      '("2S 2C AD AH AS" "7C 6S 6D 7C 7D")
                                            '("2S 2C 4D AH AS" "7C 6S 4D 7C 7D"))
    (test-one-function #'is-flush           '("3D 4D 7D 9D JD" "4H 5H 6H 7H 8H")
                                            '("3D 4D 7D 9S JD" "4H 5H 6H 7H 8D"))
    (test-one-function #'is-straight        '("AS TS KS JS QS" "3D 4H 5S 6C 7H")
                                            '("AS TS KS 3S QS" "3D 4H 5S AC 7H"))
    (test-one-function #'is-three-of-a-kind '("2S 7C AD AH AS" "7C 6S 2D 7C 7D")
                                            '("2S 2C 4D AH AS" "7C 6S 4D 3C 7D"))
    (test-one-function #'is-two-pairs       '("2S 4D 3C 2C 4S" "7S 7D JH JD 6C")
                                            '("2S 4D 2C 2C 4S" "7S 7D JH 3D 6C"))
    (test-one-function #'is-one-pair        '("2S 4D 3C 2C AS" "7S 7D 3H JD 6C")
                                            '("5S 4D 2C KC 7S" "3S 7D JH 5D 6C")))); }}}

; All the is-foo functions modify the hand they're given, setting scoring-cards
; and non-scoring-cards appropriately if they return true.

(defun is-royal-flush (hand); {{{
  (and (= (parse-card-value #\A)
          (hand-card-value hand 0))
       (is-straight-flush hand))); }}}

(defun is-straight-flush (hand); {{{
  (when (and (hand-same-suit hand)
             (hand-descending-values hand))
    (setf (hand-scoring-cards hand) (hand-cards hand)))); }}}

(defun is-four-of-a-kind (hand); {{{
  (has-n-cards-with-same-value hand 4)); }}}

(defun is-full-house (hand); {{{
  (when (and (has-n-cards-with-same-value hand 3 nil)
             (has-n-cards-with-same-value hand 2 nil))
    (setf (hand-scoring-cards hand) (hand-cards hand)))); }}}

(defun is-flush (hand); {{{
  (when (hand-same-suit hand)
    (setf (hand-scoring-cards hand) (hand-cards hand)))); }}}

(defun is-straight (hand); {{{
  (when (hand-descending-values hand)
    (setf (hand-scoring-cards hand) (hand-cards hand)))); }}}

(defun is-three-of-a-kind (hand); {{{
  (has-n-cards-with-same-value hand 3)); }}}

(defun is-two-pairs (hand); {{{
  (when (has-n-cards-with-same-value hand 2)
    (if (= 4 (length (hand-scoring-cards hand)))
      t
      (setf (hand-scoring-cards hand) nil)))); }}}

(defun is-one-pair (hand); {{{
  (has-n-cards-with-same-value hand 2)); }}}

(defun is-highest-card (hand); {{{
  "This will always succeed.  I wrote it so that I don't have to handle this
   case specially elsewhere."
  (setf (hand-scoring-cards hand) (hand-cards hand))); }}}

(defun rank-hand (hand); {{{
  "Determine the rank for a hand."
  (setf (hand-rank hand)
        (cond ((is-royal-flush hand)     10)
              ((is-straight-flush hand)  9)
              ((is-four-of-a-kind hand)  8)
              ((is-full-house hand)      7)
              ((is-flush hand)           6)
              ((is-straight hand)        5)
              ((is-three-of-a-kind hand) 4)
              ((is-two-pairs hand)       3)
              ((is-one-pair hand)        2)
              ((is-highest-card hand)    1)))); }}}

(defun rank-top-cards-helper (cards-type cards-1 cards-2); {{{
  "Return t if cards-1 is better than cards-2."
  (block compare
    (dofromto (0 (1- (length cards-1)) i)
      (let ((value-1 (card-value (aref cards-1 i)))
            (value-2 (card-value (aref cards-2 i))))
        (when (not (= value-1 value-2))
          (format t " then ~A card ~A: ~A vs ~A"
                  cards-type i value-1 value-2)
          (return-from compare (> value-1 value-2)))))
    -1)); }}}

(defun rank-top-cards (hand-1 hand-2); {{{
  "Return t if hand-1 is better than hand-2."
  (let ((result (rank-top-cards-helper "scoring"
                                       (hand-scoring-cards hand-1)
                                       (hand-scoring-cards hand-2))))
    (when (and (numberp result)
               (= result -1))
      ; Fall back to non-scoring cards.
      (setf result (rank-top-cards-helper "non-scoring"
                                          (hand-non-scoring-cards hand-1)
                                          (hand-non-scoring-cards hand-2))))
    (format t "~%")
    result)); }}}

(defun compare-hands (hand-1 hand-2); {{{
  "Return t if hand-1 is better than hand-2."
  (rank-hand hand-1)
  (rank-hand hand-2)
  (format t "~A vs ~A: rank ~A vs rank ~A"
          (hand-string-form hand-1) (hand-string-form hand-2)
          (hand-rank hand-1) (hand-rank hand-2))
  (if (= (hand-rank hand-1)
         (hand-rank hand-2))
    (rank-top-cards hand-1 hand-2)
    (progn
      (format t "~%")
      (> (hand-rank hand-1)
          (hand-rank hand-2))))); }}}

(defun project-euler-54-1 (); {{{
  (with-open-file (hands-fh #p"poker.txt")
    (count-if #'(lambda (pair)
                  (apply #'compare-hands pair))
              (read-hands-file hands-fh)))); }}}

; If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.; {{{
;
; Not all numbers produce palindromes so quickly. For example,
;
; 349 + 943 = 1292,
; 1292 + 2921 = 4213
; 4213 + 3124 = 7337
;
; That is, 349 took three iterations to arrive at a palindrome.
;
; Although no one has proved it yet, it is thought that some numbers, like 196,
; never produce a palindrome. A number that never forms a palindrome through the
; reverse and add process is called a Lychrel number. Due to the theoretical
; nature of these numbers, and for the purpose of this problem, we shall assume
; that a number is Lychrel until proven otherwise. In addition you are given
; that for every number below ten-thousand, it will either (i) become a
; palindrome in less than fifty iterations, or, (ii) no one, with all the
; computing power that exists, has managed so far to map it to a palindrome. In
; fact, 10677 is the first number to be shown to require over fifty iterations
; before producing a palindrome: 4668731596684224866951378664 (53 iterations,
; 28-digits).
;
; Surprisingly, there are palindromic numbers that are themselves Lychrel
; numbers; the first example is 4994.
;
; How many Lychrel numbers are there below ten-thousand?
;
; NOTE: Wording was modified slightly on 24 April 2007 to emphasise the
; theoretical nature of Lychrel numbers.; }}}

(defun is-palindromic-number (a-number); {{{
  (= a-number (reverse-number a-number))); }}}

(defun reverse-number (a-number); {{{
  (digits-to-number (reverse (number-to-digits a-number)))); }}}

(defun is-lychrel-number (a-number); {{{
  (let ((current-candidate (+ a-number (reverse-number a-number))))
    (block test
      ; We've already completed one iteration, and we're allowed 'less than
      ; 50 iterations'.
      (dofromto (1 48 i)
        (when (is-palindromic-number current-candidate)
          (return-from test nil))
        (setf current-candidate (+ current-candidate
                                  (reverse-number current-candidate))))
      t))); }}}

(defun project-euler-55-1 (); {{{
  (let ((num-lychrel-numbers 0))
    (dofromto (1 9999 current-number num-lychrel-numbers)
      (when (is-lychrel-number current-number)
        (incf num-lychrel-numbers))))); }}}

; A googol (10^100) is a massive number: one followed by one-hundred zeros;; {{{
; 100^100 is almost unimaginably large: one followed by two-hundred zeros.
; Despite their size, the sum of the digits in each number is only 1.
;
; Considering natural numbers of the form, a^b, where a, b < 100, what is the
; maximum digital sum?; }}}

(defun project-euler-56-1 (); {{{
  (let ((highest-sum 0))
    (dofromto (1 99 a highest-sum)
      (let ((a^b a))
        (dofromto (2 99 b)
          (setf a^b (* a^b a))
          (setf highest-sum (max highest-sum
                                 (apply #'+ (number-to-digits a^b))))))))); }}}

; It is possible to show that the square root of two can be expressed as an; {{{
; infinite continued fraction.
;
;  sqrt(2) = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
;
;  By expanding this for the first four iterations, we get:
;
;  1 + 1/2 = 3/2 = 1.5
;  1 + 1/(2 + 1/2) = 7/5 = 1.4
;  1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;  1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
;
;  The next three expansions are 99/70, 239/169, and 577/408, but the eighth
;  expansion, 1393/985, is the first example where the number of digits in the
;  numerator exceeds the number of digits in the denominator.
;
;  In the first one-thousand expansions, how many fractions contain a numerator
;  with more digits than denominator?; }}}

; Reformatting the description gives:; {{{
;  1 +                      1/2    = 3/2 = 1.5          = 1 + 1/2
;  1 +               1/(2 + 1/2)   = 7/5 = 1.4          = 1 + 2/5
;  1 +        1/(2 + 1/(2 + 1/2))  = 17/12 = 1.41666... = 1 + 5/12
;  1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379... = 1 + 12/29
; Calculating the portion after the + is easy:
;   numerator(n) = denominator(n-1)
;   denominator(n) = numerator(n-1) + 2 * denominator(n-1)
; (1 + numerator/denominator) = (numerator + denominator)/denominator, so we
; just compare (num-digits (+ numerator denominator)) against
; (num-digits denominator); }}}

(defun project-euler-57-1 (); {{{
  ; numerator and denominator are functions, but they're also the best names for
  ; the variables, so I'm using them.
  (let ((numerator 1)
        (denominator 2)
        (result 0))
    (dofromto (2 1000 i result)
      (psetf numerator denominator
             denominator (+ numerator (* 2 denominator)))
      (when (> (length (write-to-string (+ numerator denominator)))
               (length (write-to-string denominator)))
        (incf result))))); }}}

; Starting with 1 and spiralling anticlockwise in the following way, a square; {{{
; spiral with side length 7 is formed.
;
; 37 36 35 34 33 32 31
; 38 17 16 15 14 13 30
; 39 18  5  4  3 12 29
; 40 19  6  1  2 11 28
; 41 20  7  8  9 10 27
; 42 21 22 23 24 25 26
; 43 44 45 46 47 48 49
;
; It is interesting to note that the odd squares lie along the bottom right
; diagonal, but what is more interesting is that 8 out of the 13 numbers lying
; along both diagonals are prime; that is, a ratio of 8/13  62%.
;
; If one complete new layer is wrapped around the spiral above, a square spiral
; with side length 9 will be formed. If this process is continued, what is the
; side length of the square spiral for which the ratio of primes along both
; diagonals first falls below 10%?; }}}

; Diagonals are given by:; {{{
; 1 + 2 + 2 + 2 + 2
;   + 4 + 4 + 4 + 4
;   + 6 + 6 + 6 + 6
;   + 8 + 8 + 8 + 8
;   . . .; }}}

(defun project-euler-58-1 (); {{{
  (do ((num-primes 0)
       (num-diagonals 1)
       (index 1)
       (increment 2 (+ 2 increment)))
      ((and (not (zerop num-primes))
            (< (/ num-primes num-diagonals) 0.1))
       (1- increment))
    (dofromto (1 4 i)
      (incf index increment)
      (incf num-diagonals)
      ; The fourth corner is always a square, so cannot be a prime.
      (when (and (not (= 4 i))
                 (primep index))
        (incf num-primes))))); }}}

(defun sort-hash-keys-by-value (hash); {{{
  "Sort the keys of a hash by their associated value."
  (let ((keys '()))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (push key keys))
             hash)
    (sort keys #'> :key #'(lambda (key) (gethash key hash))))); }}}

; Each character on a computer is assigned a unique code and the preferred; {{{
; standard is ASCII (American Standard Code for Information Interchange). For
; example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
;
; A modern encryption method is to take a text file, convert the bytes to ASCII,
; then XOR each byte with a given value, taken from a secret key. The advantage
; with the XOR function is that using the same encryption key on the cipher
; text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 =
; 65.
;
; For unbreakable encryption, the key is the same length as the plain text
; message, and the key is made up of random bytes. The user would keep the
; encrypted message and the encryption key in different locations, and without
; both "halves", it is impossible to decrypt the message.
;
; Unfortunately, this method is impractical for most users, so the modified
; method is to use a password as a key. If the password is shorter than the
; message, which is likely, the key is repeated cyclically throughout the
; message. The balance for this method is using a sufficiently long password key
; for security, but short enough to be memorable.
;
; Your task has been made easy, as the encryption key consists of three lower
; case characters. Using cipher1.txt (right click and 'Save Link/Target As...'),
; a file containing the encrypted ASCII codes, and the knowledge that the plain
; text must contain common English words, decrypt the message and find the sum
; of the ASCII values in the original text.; }}}

(defun 8-bit-int-to-bit-array (int); {{{
  "Convert an 8-bit integer into an array of bits."
  (let ((bit-array (make-array 8 :element-type 'bit))
        (remainder int))
    (dofromto (0 7 i bit-array)
      (setf (bit bit-array i) (mod remainder 2))
      (setf remainder (floor remainder 2))))); }}}

(defun bit-array-to-8-bit-int (bit-array); {{{
  "Convert an array of bits into an 8-bit integer."
  (let ((int 0))
    (dofromto (7 0 i int)
      (setf int (+ (* int 2) (bit bit-array i)))))); }}}

(defun xor-ints (int1 int2); {{{
  "xor two ints, returning an int."
  (bit-array-to-8-bit-int (bit-xor (8-bit-int-to-bit-array int1)
                                   (8-bit-int-to-bit-array int2)))); }}}

(defun search-for-string (encrypted-letters key-length a-string); {{{
  "Search the list encrypted-letters for the characters in a-string."
  ; The key found by this function when a-string is " the " is correct, but
  ; that's good luck.  This should return a list of keys, then let the calling
  ; function work out which one is correct.
  (let ((haystack (coerce encrypted-letters 'array))
        (needle (map 'array #'(lambda (x) (char-code x)) a-string))
        (lowercase-letters '())
        (key (make-array key-length)))
    (dofromto ((char-code #\z) (char-code #\a) letter)
      (push letter lowercase-letters))
    (block find-key
      (dofromto (0 (1- (- (length haystack) (length needle))) index)
        (block try-at-position-n
          ; We don't need to brute-force the first key-length keys:
          ;   encrypted-letter xor expected-letter == key-letter
          ; Each key-letter must be a lowercase letter; if not, we don't have a
          ; match here, move to the next position.
          (dofromto (0 (1- key-length) offset)
            (setf (aref key offset) (xor-ints (aref haystack (+ index offset))
                                              (aref needle offset)))
            (when (not (member (aref key offset) lowercase-letters))
              (return-from try-at-position-n)))
          ; Now, check if the remaining characters are successfully decrypted
          ; using the key we've just built.  We need at least key-length more
          ; characters in the search string to verify the key, but we don't
          ; enforce that.
          (dofromto (key-length (1- (length needle)) offset)
            (when (not (equal (aref needle offset)
                              (xor-ints (aref haystack (+ index offset))
                                        (aref key (mod offset key-length)))))
              (return-from try-at-position-n)))
          ; We've found the key!
          (return-from find-key key)))))); }}}

(defun project-euler-59-1 (); {{{
  (let* ((encrypted-letters (read-comma-delimited-file #p"cipher1.txt"))
         (key-length 3)
         (key (search-for-string encrypted-letters key-length " the "))
         (message '()))
    (let ((index 0))
      (dolist (encrypted-letter encrypted-letters)
        (push (xor-ints (aref key index) encrypted-letter) message)
        (setf index (mod (1+ index) 3))))
    (setf message (nreverse message))
    (format t "~{~A~}~%" (mapcar #'(lambda (int) (code-char int)) message))
    (apply #'+ message))); }}}

; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes; {{{
; and concatenating them in any order the result will always be prime. For
; example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four
; primes, 792, represents the lowest sum for a set of four primes with this
; property.
;
; Find the lowest sum for a set of five primes for which any two primes
; concatenate to produce another prime.; }}}

(defun find-concatonating-prime-pairs (sieve); {{{
  "Find pairs (ab cd) such that ab, cd, abcd, and cdab are prime.  Returns a
   hash table mapping (prime -> list of primes), e.g.
   #S(HASH-TABLE (ab . (cd)) (cd . (ab)))"
  (let ((prime-pairs (make-hash-table))
        (upper-bound (1- (array-dimension sieve 0))))
    (dofromto (2 upper-bound prime1 prime-pairs)
      (when (is-prime sieve prime1)
        (dofromto (prime1 upper-bound prime2)
          (when (is-prime sieve prime2)
            (let* ((digits1 (number-to-digits prime1))
                   (digits2 (number-to-digits prime2))
                   (concat-digits1 (concatenate 'list digits1 digits2))
                   (concat-digits2 (concatenate 'list digits2 digits1))
                   (concat-number1 (digits-to-number concat-digits1))
                   (concat-number2 (digits-to-number concat-digits2)))
              (when (and (not (= prime1 prime2))
                         ; Numbers with leading digits confuse things.
                         (not (= 0 (first concat-digits1)))
                         (not (= 0 (first concat-digits2)))
                         (is-prime sieve concat-number1)
                         (is-prime sieve concat-number2))
                (push prime2 (gethash prime1 prime-pairs '()))
                (push prime1 (gethash prime2 prime-pairs '())))))))))); }}}

(defun make-pair-graphs (hash); {{{
  "Returns a deduplicated list of pairs.  Each pair will have the lower-valued
   element first."
  (let ((sub-graphs '()))
    (maphash #'(lambda (node-1 list-of-nodes)
                 (dolist (node-2 list-of-nodes)
                   (push (sort (list node-1 node-2) #'<) sub-graphs)))
             hash)
    (delete-duplicates sub-graphs :test #'equal))); }}}

(defun generate-n+1-node-graphs (n-node-graphs pairs-hash); {{{
  "Generate n+1 node graphs from n-node graphs.  Returns a deduplicated list.
   The nodes in each graph are in ascending order."
  (let ((n+1-node-graphs '()))
    (dolist (n-node-graph n-node-graphs)
      (let ((first-node (first n-node-graph))
            (other-nodes (rest n-node-graph)))
        (dolist (possibly-connected-node (gethash first-node pairs-hash))
          (block next-node
            (dolist (connected-node other-nodes)
              (when (not (member possibly-connected-node
                                 (gethash connected-node pairs-hash)))
                (return-from next-node)))
            (push (sort (append n-node-graph (list possibly-connected-node))
                        #'<)
                  n+1-node-graphs)))))
    (delete-duplicates n+1-node-graphs :test #'equal))); }}}

(defun project-euler-60-1 (&optional (upper-bound 10000)); {{{
  (let* ((sieve (sieve-of-eratosthenes upper-bound))
         (pairs-hash (find-concatonating-prime-pairs sieve))
         (graphs (make-pair-graphs pairs-hash)))
    (dofromto (3 5 i)
      (setf graphs (generate-n+1-node-graphs graphs pairs-hash)))
    (first (sort (mapcar #'(lambda (graph)
                             (apply #'+ graph))
                         graphs)
                 #'<)))); }}}

(defun hash-keys (hash); {{{
  "Return an unsorted list of hash keys."
  (let ((keys '()))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (push key keys))
             hash)
    keys)); }}}

(defun hash-values (hash); {{{
  "Return an unsorted, non-deduplicated list of hash values."
  (let ((results '()))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (push value results))
             hash)
    results)); }}}

(defun remove-reversed-sequences (sequences); {{{
  "Given a list of sequences, remove any sequence X where we've already
   seen (reverse X).  Returns a new list, preserving order."
  (let ((seen (make-hash-table :test #'equal))
        (results '()))
    (dolist (seq sequences)
      (unless (gethash (reverse seq) seen nil)
        (push seq results)
        (setf (gethash seq seen) t)))
    (nreverse results))); }}}

; Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are; {{{
; all figurate (polygonal) numbers and are generated by the following formulae:
;
; Triangle    P3,n=n(n+1)/2    1, 3, 6, 10, 15, ...
; Square      P4,n=n*2         1, 4, 9, 16, 25, ...
; Pentagonal  P5,n=n(3n-1)/2   1, 5, 12, 22, 35, ...
; Hexagonal   P6,n=n(2n-1)     1, 6, 15, 28, 45, ...
; Heptagonal  P7,n=n(5n-3)/2   1, 7, 18, 34, 55, ...
; Octagonal   P8,n=n(3n-2)     1, 8, 21, 40, 65, ...
; The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three
; interesting properties.
;
; * The set is cyclic, in that the last two digits of each number is the first
;   two digits of the next number (including the last number with the first).
; * Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and
;   pentagonal (P5,44=2882), is represented by a different number in the set.
; * This is the only set of 4-digit numbers with this property.
;
; Find the sum of the only ordered set of six cyclic 4-digit numbers for which
; each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and
; octagonal, is represented by a different number in the set.; }}}

(defun square-number (n); {{{
  "Return the nth square number."
  (* n n)); }}}

(defun heptagonal-number (n); {{{
  "Return the nth heptagonal number."
  (/ (* n (- (* 5 n) 3)) 2)); }}}

(defun octagonal-number (n); {{{
  "Return the nth octagonal number."
  (* n (- (* n 3) 2))); }}}

(defun make-4-digit-polygonal-numbers (func); {{{
  "Returns a list of 4 digit polygonal numbers created by successively calling
   (funcall func n)."
  (do* ((numbers '())
        (n 1 (1+ n))
        (latest-number (funcall func n) (funcall func n)))
       ((>= latest-number 10000) (nreverse numbers))
    (when (>= latest-number 1000)
      (push latest-number numbers)))); }}}

(defun extract-ab (a-number); {{{
  "Returns ab from abcd."
  (digits-to-number (subseq (number-to-digits a-number) 0 2))); }}}

(defun extract-cd (a-number); {{{
  "Returns cd from abcd."
  (digits-to-number (subseq (number-to-digits a-number) 2 4))); }}}

(defun follow-4-digit-chain-link (left-links right-links); {{{
  "The number abcd chains with the number cdef.  Given the lists left-links
   and right-links, this function will remove any left links that don't have
   corresponding right links and vice versa, returning new lists."
  (let ((cd-hash (make-hash-table))
        (ab-hash (make-hash-table))
        (new-left-list)
        (new-right-list))
    (dolist (left left-links)
      (setf (gethash (extract-cd left) cd-hash) t))
    (dolist (right right-links)
      (setf (gethash (extract-ab right) ab-hash) t))
    (setf new-left-list
          (remove-if-not #'(lambda (key)
                             (gethash (extract-cd key) ab-hash))
                         left-links))
    (setf new-right-list
          (remove-if-not #'(lambda (key)
                             (gethash (extract-ab key) cd-hash))
                         right-links))
    (values new-left-list new-right-list))); }}}

(defun follow-4-digit-chain-link-wrapper (chain left-index right-index); {{{
  "A wrapper around follow-4-digit-chain-link to run it on two elements from an
   array, replacing the original values."
  (multiple-value-bind (left-links right-links)
                       (follow-4-digit-chain-link (aref chain left-index)
                                                  (aref chain right-index))
    (setf (aref chain left-index) left-links
          (aref chain right-index) right-links))); }}}

(defun remove-broken-4-digit-chain-links (number-chain); {{{
  "Traverse a 4-digit chain, removing broken links.  The lists in the array
   number-chain will be modified."
  (let ((last-index (1- (length number-chain)))
        (num-links-in-index-0 (length (aref number-chain 0))))
    (dofromto (0 (1- last-index) i)
      (follow-4-digit-chain-link-wrapper number-chain i (1+ i)))
    (follow-4-digit-chain-link-wrapper number-chain last-index 0)
    (dofromto ((1- last-index) 0 i)
      (follow-4-digit-chain-link-wrapper number-chain i (1+ i)))
    ; We leave broken links when the following happens:
    ; 1 Traverse the chain in one direction, removing broken links.
    ; 2 Wrap around from index X to index 0, removing elements from index 0.
    ;   There are now elements in index 1 that don't have a link in index 0.
    ;   The only way to fix this is to traverse the chain again.
    ; Recurse when index 0 still has elements and we removed some elements from
    ; index 0.
    (when (and (not (zerop (length (aref number-chain 0))))
               (not (= (length (aref number-chain 0)) num-links-in-index-0)))
      (remove-broken-4-digit-chain-links number-chain)))); }}}

(defun project-euler-61-1 (); {{{
  (let* ((polygonal-functions (list #'octagonal-number
                                    #'triangle-number
                                    #'square-number
                                    #'pentagonal-number
                                    #'hexagonal-number
                                    #'heptagonal-number))
         (num-polygonal-numbers (length polygonal-functions))
         (polygonal-numbers (make-array num-polygonal-numbers))
         (chains '()))
    ; Populate the array of polygonal numbers.
    (let ((i 0))
      (dolist (func polygonal-functions)
        (setf (aref polygonal-numbers i)
              (make-4-digit-polygonal-numbers func))
        (incf i)))

    ; Generate all possible permutations of the list of polygonal numbers,
    ; excluding octagonal.  We start every chain with the octagonal numbers (any
    ; group would work), because '(0 1 2 3 4 5) rotated any number of times is
    ; the same chain, and there's no point in processing each chain six times.
    (dolist (permutation (permute '(1 2 3 4 5)))
      (let ((number-chain (make-array num-polygonal-numbers))
            (dest-index 0))
        (dolist (src-index (cons 0 permutation))
          (setf (aref number-chain dest-index)
                (copy-seq (aref polygonal-numbers src-index)))
          (incf dest-index))
        (remove-broken-4-digit-chain-links number-chain)
        (when (not (zerop (length (aref number-chain 0))))
          (push number-chain chains))))

    (if (= 1 (length chains))
      (let ((sum 0)
            (chain (first chains)))
        (dotimes (index (length chain) sum)
          (incf sum (first (aref chain index)))))
      (progn
        (format t "expecting one chain, found ~A~%" (length chains))
        chains)))); }}}

; The cube, 41063625 (345^3), can be permuted to produce two other cubes:; {{{
; 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube
; which has exactly three permutations of its digits which are also cube.
;
; Find the smallest cube for which exactly five permutations of its digits are
; cube.; }}}

; Observations:; {{{
; * There are 40320 permutations of an 8 digit number, so checking all
;   permutations of each cube is infeasable.
; * All permutations have the same length.
;
; First idea:
; 1 Use a hash table to store permutations
; 2 Write an infinite loop, calculating (cube n).  Split (cube n) into digits,
;   sort the digits, and (push (cube n) (gethash sorted-digits permutations)).
; 3 When the number of digits in (cube n) increases, we've found all cubes of
;   length x, so for any n, we have all permutations of (cube n) that are
;   themselves cubes.  Iterate over the hash table, looking for an entry with
;   exactly 5 permutations.  The smallest number in that entry is the result.
;}}}

(defun find-keys-with-n-values (hash n); {{{
  "Find all keys in hash whose value contains n items.  Items can be stored in
   lists, hashes, or anything else #'length works on."
  (let ((results '()))
    (maphash #'(lambda (key value)
                 (when (= n (length value))
                   (push key results)))
             hash)
    results)); }}}

(defun pick-smallest-cube (cube-permutations n); {{{
  "Pick the smallest cube with n permutations."
  (let ((keys (find-keys-with-n-values cube-permutations n))
        (permutations '()))
    (mapcar #'(lambda (key)
                 (push (sort (gethash key cube-permutations) #'<) permutations))
             keys)
    (sort permutations #'< :key #'first)
    (first permutations))); }}}

(defun project-euler-62-1 (); {{{
  (do* ((cube-permutations (make-hash-table :test #'equal))
        (n 1 (1+ n))
        (n-cubed (* n n n) (* n n n))
        (n-cubed-hash-key (sort (number-to-digits n-cubed) #'<)
                          (sort (number-to-digits n-cubed) #'<))
        (key-length 0)
        (result nil))
      (result result)
    (push n-cubed (gethash n-cubed-hash-key cube-permutations '()))
    (when (> (length n-cubed-hash-key) key-length)
      (setf key-length (length n-cubed-hash-key))
      (setf result (pick-smallest-cube cube-permutations 5))))); }}}
