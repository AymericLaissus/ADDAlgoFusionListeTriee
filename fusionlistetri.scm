(define fusltri 
    (lambda (L1 L2)
        (cond ((null? L1) L2)
            ((null? L2) L1)
            ((< (car L1) (car L2)) (cons (car L1) (fusltri (cdr L1) L2)))
            (else (cons (car L2) (fusltri (cdr L2) L1))))))