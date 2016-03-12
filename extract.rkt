#lang racket

;'(
;  (NP
;   (NP (VBN Activated)
;       (NNP CD)
;       (CD 3+)
;       (JJ enriched)
;       (JJ human)
;       (JJ peripheral)
;       (NN blood)
;       (NN T)
;       (NNS cells))
;   (VP (VBD exhibited)
;       (NP (NP (JJ potent)
;               (NN capacity))
;           (PP (IN for)
;               (NP (JJ transendothelial)
;                   (NN migration))))
;       (PP (IN through)
;           (NP (NNP HUVEC)
;               (NNS layers)))
;       (PP (IN in)
;           (NP (NP (DT the)
;                   (NN absence))
;               (PP (IN of)
;                   (NP (NNP T)
;                       (NN cell))))))
;   (X (SYM ***))) )
;
;"Activated CD 3+ enriched human peripheral blood T cells exhibited potent capacity for transendothelial migration through HUVEC 
;layers in the absence of T cell ***."

(define-struct noun (name order) #:transparent)

(define (tokenize-nouns parse-tree [count 0])
  (letrec ([tokenize (lambda (parse-tree) 
                       (cond
                         [(empty? parse-tree) empty]
                         [(empty? (rest parse-tree)) empty]
                         [(list? (second parse-tree)) (append (tokenize (second parse-tree))
                                                              (tokenize (rest parse-tree)))]
                         [(or (equal? (first parse-tree) 'NN)
                              (equal? (first parse-tree) 'NNP)
                              (equal? (first parse-tree) 'NNS)) (set! count (add1 count))
                                                                (cons (make-noun (symbol->string (second parse-tree)) count)
                                                                      (tokenize (rest parse-tree)))]
                         [else (set! count (add1 count))
                               (tokenize-nouns (rest parse-tree))]))])
    (tokenize parse-tree)))

(define (tokenize-input)
  (define tree (read))
  (if (eof-object? tree)
      (void)
      (begin
        (extract 4
                 (tokenize-nouns (first tree)))
        (tokenize-input))))

(define (extract distance tokens)
  (if (empty? tokens)
      (void)
      (let ([curr-loc (noun-order (first tokens))]
            [curr-word (noun-name (first tokens))])
        (map (lambda (n)
               (if (<= (- (noun-order n) curr-loc) distance)
                   (printf "~a ~a~n"
                           curr-word (noun-name n))
                   (void))) (rest tokens))
        (extract distance (rest tokens)))))

(tokenize-input)
