;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Siddhant Mahajan (20889590)
;; CS 135 Fall 2020
;; Assignment 07, Problem 02
;; ***************************************************
;;

(require "animals.rkt")


;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)



;; *********************
;; Problem 2a
;; *********************


;; (collect-attributed examples) produces a list of symbols with all the
;;      unique attributes from a list of Examples, examples
;; Examples:
(check-expect (collect-attributes empty) empty)
(check-expect (collect-attributes '((crow large flies angry)
                                    (sparrow small flies)))
              '(large angry small flies))
(check-expect (collect-attributes '((crow large flies angry)))
              '(large flies angry))


;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local [(define (combine-attributes lst)
            (cond [(empty? lst) empty]
                  [else (append (rest (first lst))
                                (combine-attributes (rest lst)))]))
          (define (remove-duplicates lst)
            (cond [(empty? lst) empty]
                  [(member? (first lst)
                            (rest lst)) (remove-duplicates (rest lst))]
                  [else (cons (first lst)
                              (remove-duplicates (rest lst)))]))]
    (remove-duplicates (combine-attributes examples))))


;; Tests:
(check-expect (collect-attributes '((goose swims flies angry large)
                                    (goose swims flies angry large)))
              '(swims flies angry large))
(check-expect (collect-attributes '((goose swims flies large)
                                    (rabbit medium angry)))
              '(swims flies large medium angry))
(check-expect (collect-attributes '((crow flies small)
                                    (goose angry large)
                                    (duck swims small)))
              '(flies angry large swims small))




;; *********************
;; Problem 2b
;; *********************


;; (split-examples examples symbol) produces a list of two lists of
;;      examples, one which contains the Sym, symbol and one that does
;;      not contain the Sym, symbol from a list of Example, examples
;; Examples:
(check-expect (split-examples empty 'goose)
              (list empty empty))

(check-expect (split-examples '((goose large swims flies angry)) 'goose)
              (list (list '(goose large swims flies angry)) empty))

(check-expect (split-examples '((goose large swims flies angry)) 'crow)
              (list empty (list '(goose large swims flies angry))))

(check-expect (split-examples '((goose large swims flies angry)
                                (squirrel small angry)) 'goose)
              (list (list '(goose large swims flies angry))
                    (list '(squirrel small angry))))


;; split-examples: (listof Example) Sym ->
;;                 (list (listof Example) (listof Example))
(define (split-examples examples symbol)
  (list
   (filter (lambda (x) (member? symbol x)) examples)
   (filter (lambda (x) (not (member? symbol x))) examples)))


;; Tests:
(check-expect (split-examples '((squirrel small angry)
                                (crow medium flies angry)
                                (goose large swims flies angry)) 'small)
              '(((squirrel small angry))
                ((crow medium flies angry)
                 (goose large swims flies angry))))





;; *********************
;; Problem 2c
;; *********************


;; (histogram examples) produces a histogram based on a given list of
;;      Example, examples
;; Examples:
(check-expect (histogram empty) empty)
(check-expect (histogram '((crow flies angry))) '((flies 1) (angry 1)))
(check-expect (histogram '((crow flies angry)
                           (goose flies swims large angry)))
              '((flies 2) (swims 1) (large 1) (angry 2)))



;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local [(define seen-attributes (collect-attributes examples))

          (define (combine-attributes lst)
            (cond [(empty? lst) empty]
                  [else (append (rest (first lst))
                                (combine-attributes (rest lst)))]))

          (define (count-occurence sym lst)
            (cond [(empty? lst) 0]
                  [(symbol=? sym (first lst))
                   (add1 (count-occurence sym (rest lst)))]
                  [else (count-occurence sym (rest lst))]))

          (define (create-list lst examples)
            (cond [(empty? lst) empty]
                  [else (cons
                         (list (first lst)
                               (count-occurence
                                (first lst)
                                (combine-attributes examples)))
                         (create-list (rest lst) examples))]))]
    
    (create-list seen-attributes examples)))


;; Tests:
(check-expect (histogram '((goose large angry flies swims)
                           (squirrel small angry)
                           (rabbit medium angry)
                           (duck medium swims)
                           (deer large)))
              '((flies 1) (small 1) (angry 3) (medium 2)
                          (swims 2) (large 2)))

(check-expect (histogram '((goose large angry flies swims)
                           (squirrel small angry)
                           (rabbit medium angry)
                           (duck medium swims)
                           (deer large)
                           (moose large angry)))
              '((flies 1) (small 1) (medium 2)
                          (swims 2) (large 3) (angry 4)))





;; *********************
;; Problem 2d
;; *********************


;; (augment-histogram histogram attributes total) consumes a Histogram,
;;      histogram and produces an Augmented Histogram based on the
;;      list of symbols, attributes and the total number of examples, total
;; Examples:
(check-expect (augment-histogram empty empty 20) empty) 
(check-expect (augment-histogram '((a 100))'(a b) 100)
              '((a 100 0) (b 0 100)))


;; augment-histogram: Histogram (listof Sym) Nat -> AH
(define (augment-histogram histogram attributes total)
  (local [(define (get-count histogram symbol)
            (cond [(empty? histogram) 0]
                  [(symbol=? symbol (first (first histogram)))
                   (second (first histogram))]
                  [else (get-count (rest histogram) symbol)]))

          (define symbol-1
            (cond [(empty? attributes) false]
                  [else (first attributes)]))
          
          (define symbol-count
            (cond [(boolean? symbol-1) false]
                  [else (get-count histogram symbol-1)]))]

    (cond [(empty? attributes) empty]
          [else (cons (list symbol-1 symbol-count (- total symbol-count))
                      (augment-histogram histogram (rest attributes)
                                         total))])))


;; Tests:
(check-expect (augment-histogram '((a 100) (c 50)) '(a b c) 200)
              '((a 100 100) (b 0 200) (c 50 150)))
(check-expect (augment-histogram empty '(x y) 10)
              '((x 0 10) (y 0 10)))





;; *********************
;; Problem 2e
;; *********************


;; (entropy positive-counts negative-counts) produces the entropy of two
;;      elements of an Augmented Histogram (AH)
;; Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              #i0.5663948489858 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361))
              #i0.5825593868115 0.001)
(check-within (entropy (list 'a 0 100) (list 'a 100 0))
              0.0 0.001)
(check-within (entropy (list 'a 0 100) (list 'a 0 100))
              #i1.0 0.001)


;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) -> Num
;; Requires: (first positive-counts) = (first negative-counts)
(define (entropy positive-counts negative-counts)
  (local [(define a (second positive-counts))
          (define b (second negative-counts))
          (define c (third positive-counts))
          (define d (third negative-counts))

          (define (probability n m)
            (cond [(> (+ n m) 0) (/ n (+ n m))]
                  [else 0.5]))

          (define (get-entropy p)
            (cond [(= p 0) 0]
                  [else (- (* p (log p 2)))]))]

    (+ (* (probability (+ a b) (+ c d))
          (+ (get-entropy (probability a b))
             (get-entropy (probability b a))))
       (* (probability (+ c d) (+ a b))
          (+ (get-entropy (probability c d))
             (get-entropy (probability d c)))))))




;; *********************
;; Problem 2f
;; *********************


;; (entropy-attributes positive negative) produces an EAL based on two
;;      provided Augmented Histograms, positive and neagtive
;; Examples:
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              
              (list
               (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
               (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
               (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))

              0.001)


;; entropy-attributes: AH AH -> EAL
;; Requires: attributes in positive and negative appear in the same order
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive)
                                   (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))






;; *********************
;; Problem 2g
;; *********************


;; (best-attribute entropies) produces the attribute with the least
;;      entropy from an EAL, entropies
;; Examples:
(check-expect (best-attribute '((a 0.2))) 'a)
(check-expect (best-attribute '((4 0.2) (3 0.19))) 3)
(check-expect (best-attribute '((4 0.19) (3 0.19))) 4)
(check-expect (best-attribute '((4 0.19) (3 0.2))) 4)

;; best-attribute: EAL -> Sym
;; Requires: entropies is not empty 
(define (best-attribute entropies)
  (local [(define (get-entropy-list entropies)
            (cond [(empty? entropies) empty]
                  [else (cons (second (first entropies))
                              (get-entropy-list (rest entropies)))]))

          (define (minimum lon)
            (cond [(empty? (rest lon)) (first lon)]
                  [else (min (first lon) (minimum (rest lon)))]))

          (define best-entropy (minimum (get-entropy-list entropies)))

          (define (get-symbol num entropies)
            (cond [(empty? (rest entropies)) (first (first entropies))]
                  [(= num (second (first entropies)))
                   (first (first entropies))]
                  [else (get-symbol num (rest entropies))]))]

    (get-symbol best-entropy entropies)))
    


;; Tests:
(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858)
                (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115)
                (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730)
                (list 'medium #i0.6901071708677))) 'large)

(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858)
                (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115)
                (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730)
                (list 'medium #i0.11))) 'medium)



;; *********************
;; Problem 2h
;; *********************


;; (build-dt examples label) produces a decision tree for a Symbol, label
;;      from a list of Examples, examples
;; Examples:
(check-expect (build-dt (random-animals 1000) 'emu) false)
(check-expect (build-dt '((goose flies)) 'goose) true)


;; build-dt: (listof Example) Symbol -> DT
(define (build-dt examples label)
  (local
    [(define attributes (collect-attributes examples))

     (define split-list (split-examples examples label))


     (define positive-examples (first split-list))
     (define negative-examples (second split-list))

     (define total (length examples))
     (define len-positive (length positive-examples))
     (define len-negative (length negative-examples))

     (define positive-histogram (histogram positive-examples))
     (define negative-histogram (histogram negative-examples))

     (define (remove-from-list attribute examples)
       (cond [(empty? examples) empty]
             [else (cons (remove-from-example attribute (first examples))
                         (remove-from-list attribute (rest examples)))]))

     (define (remove-from-example attribute example)
       (cond [(symbol=? attribute (first example)) (rest example)]
             [else (cons (first example)
                         (remove-from-example
                          attribute
                          (rest example)))]))]

    (cond [(empty? positive-examples) false]
          [(empty? negative-examples) true]
          [(empty? attributes)
           (cond [(> len-positive len-negative) true]
                 [else false])]

          [else

           (local
             [(define root
                (best-attribute
                 (entropy-attributes
                  (augment-histogram positive-histogram
                                     attributes len-positive)
                  (augment-histogram negative-histogram
                                     attributes len-negative))))
                   
              (define split-root (split-examples examples root))
              (define with-root (first split-root))
              (define without-root (second split-root))

              (define subtree1 (build-dt (remove-from-list root with-root)
                                         label))
              (define subtree2 (build-dt without-root label))]

             (cond [(equal? subtree1 subtree2) subtree1]
                   [else (list root subtree1 subtree2)]))])))



;; *********************
;; Problem 2i
;; *********************


;; (train-classifier examples label) produces a predicate that can be used
;;      to identify is a list of sttributes correspond to a Sym, label

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local [(define tree (build-dt examples label))

          (define (get-predicate dt)
            (lambda (x)
              (or (and (member? (first dt) x)
                       (eval-predicate (second dt) x))
                  (and (not (member? (first dt) x))
                       (eval-predicate (third dt) x)))))

          (define (eval-predicate dt los)
            (cond [(boolean? dt) dt]
                  [else (or (and (member? (first dt) los)
                                 (eval-predicate (second dt) los))
                            (and (not (member? (first dt) los))
                                 (eval-predicate (third dt) los)))]))]
    (get-predicate tree)))
    



;; Tests:
(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)

(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)

(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)




;; ************
;; *********************
;; Problem 3
;; *********************
;; ************



;; (performance classifier? examples label) produces a list containing the
;;      sensitivity and the specificity of a classifier.




;; performance: ((listof Sym) -> Bool) (listof Example) Sym ->
;;              (list Sym Nat Nat)
;; Requires: label is the same as the Symbol that is used to 
;;           train the classifier
(define (performance classifier? examples label)
  (local [(define positive-count
            (length (filter (lambda (x) (member? label x)) examples)))
          
          (define negative-count
            (length (filter (lambda (x) (not (member? label x))) examples)))

          (define (count-accumulator classifier? examples label pos neg)
            (cond [(empty? examples) (list pos neg)]
                  [(and (classifier? (rest (first examples)))
                        (symbol=? label (first (first examples))))
                   (count-accumulator
                    classifier? (rest examples) label (add1 pos) neg)]
                  [(and (not (classifier? (rest (first examples))))
                        (not (symbol=? label (first (first examples)))))
                   (count-accumulator
                    classifier? (rest examples) label pos (add1 neg))]
                  [else
                   (count-accumulator
                    classifier? (rest examples) label pos neg)]))

          (define sens-spec-list
            (count-accumulator classifier? examples label 0 0))]

    (list label
          (round (* 100 (/ (first sens-spec-list) positive-count)))
          (round (* 100 (/ (second sens-spec-list) negative-count))))))
