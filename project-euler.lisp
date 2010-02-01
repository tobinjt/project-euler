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
