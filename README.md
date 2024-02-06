# fizzbuzz

It's fizzbuzz, except we define our own DSL called fizzler so the actual program looks like this:

```
1:100
|%3|Fizz
|%5|Buzz
~.
```

Which then transpiles into something like this:

```lisp
(LET ((#:AGGREGATOR319 (LAMBDA (#:ARG320) (PRINC #:ARG320) (TERPRI))))
  (FUNCALL
   (LAMBDA (#:AGGREGATOR322)
     (LOOP FIZZBUZZ::FOR #:INDEX321 FIZZBUZZ::FROM 1 FIZZBUZZ::TO 100
           DO (FUNCALL
               (LAMBDA (#:INPUT323 #:AGGREGATOR324)
                 (LET (#:HAS-MATCH325)
                   (FUNCALL #:AGGREGATOR324
                            (CONCATENATE 'STRING
                                         (WHEN
                                             (FUNCALL
                                              (LAMBDA (#:X326)
                                                (= 0 (MOD #:X326 3)))
                                              #:INPUT323)
                                           (SETF #:HAS-MATCH325 T)
                                           (PRINC-TO-STRING
                                            (FUNCALL
                                             (LAMBDA (#:X327)
                                               (DECLARE (IGNORABLE #:X327))
                                               "Fizz")
                                             #:INPUT323)))
                                         (WHEN
                                             (FUNCALL
                                              (LAMBDA (#:X328)
                                                (= 0 (MOD #:X328 5)))
                                              #:INPUT323)
                                           (SETF #:HAS-MATCH325 T)
                                           (PRINC-TO-STRING
                                            (FUNCALL
                                             (LAMBDA (#:X329)
                                               (DECLARE (IGNORABLE #:X329))
                                               "Buzz")
                                             #:INPUT323)))
                                         (UNLESS #:HAS-MATCH325
                                           (PRINC-TO-STRING
                                            (FUNCALL
                                             (LAMBDA (#:X330)
                                               (DECLARE (IGNORABLE #:X330))
                                               #:X330)
                                             #:INPUT323)))))))
               #:INDEX321 #:AGGREGATOR322)))
   #:AGGREGATOR319))
```
