#lang simply-scheme

; Question 2
(define Q1_TEST_INPUT `(2 3 4 5))

(define (square num) (* num num))

(define (squares nums)
    (cond 
        ((empty? nums) `())
        (else
            (se (square (first nums) ) (squares (bf nums)))
        )
    )
)

(display (squares Q1_TEST_INPUT))
(newline)

; Question 3
(define Q3_TEST_INPUT `(you told me that I should wake you up))

(define (switch-first-word word)
    (cond
        ((equal? word `you) `i)
        (else word)
    )
)

(define (switch-word word)
    (cond
        ((equal? word `i) `you)
        ((equal? word `me) `you)
        ((equal? word `you) `me)
        (else word)
    )
)

(define (switch-helper sentence)
    (cond
        ((empty? sentence) `())
        (else (se 
                (switch-word (first sentence))
                (switch-helper (bf sentence))
        ))
    )
)


(define (switch sentence)
 (cond
    ((empty? sentence) `())
    (else (se 
            (switch-first-word (first sentence))
            (switch-helper (bf sentence))
    ))
 )
)

(display (switch Q3_TEST_INPUT))
(newline)

; Question 4

(define (ordered? nums)
    (cond 
        ((<= (count nums) 1) #t)
        (
            (>=
                (first nums)
                (first (bf nums))
            )
            #f
        )
        (else (ordered? (bf nums)))
    )
)

(ordered? `(1 2 3 4 5))
(newline)
(ordered? `(5 2 3 4 5))
(newline)
(ordered? `(1 2 4 4 5))
(newline)
(ordered? `())
(newline)
(ordered? `(1))
(newline)

; Question 5
(define (last-word-e? word) (equal? (last word) `e))

(define (ends-e-word word)
    (cond
        ((last-word-e? word) word)
        (else `())
    )
)

(define (ends-e sentence)
    (cond
        ((<= (count sentence) 1) (ends-e-word sentence))
        (else (se
            (ends-e-word (first sentence))
            (ends-e (bf sentence))
        ))
    )
)

(displayln (ends-e '(please put the salami above the blue elephant)))