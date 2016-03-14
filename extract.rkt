#lang racket

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
                                                                (cons (make-noun (if (symbol? (second parse-tree))
                                                                                     (symbol->string (second parse-tree))
                                                                                     (second parse-tree)) count)
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
