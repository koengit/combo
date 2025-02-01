#lang racket
(require redex)

(define-language L
  (e ::= (and e ...) (if e e e) x b (action e s) (not e))
  (b ::= tt ff ⊥)
  (x ::= variable-not-otherwise-mentioned)
  (p ::= ((x = e) ...))
  (s ::= string)
  (E ::= (and e ... E e ...) (if E e e) (action E s) (not E) hole))

(define-judgment-form L
  #:mode (e--> I O)
  #:contract (e--> e e)

  [----- "and tt"
   (e--> (in-hole E (and tt ...))
         (in-hole E tt))]

  [----- "and ff"
   (e--> (in-hole E (and e_1 ... ff e_2 ...))
         (in-hole E ff))]

  [----- "if tt"
   (e--> (in-hole E (if tt e_t e_f))
         (in-hole E e_t))]

  [----- "if ff"
   (e--> (in-hole E (if ff e_t e_f))
         (in-hole E e_f))]

  [----- "not tt"
   (e--> (in-hole E (not tt))
         (in-hole E ff))]

  [----- "not ff"
   (e--> (in-hole E (not ff))
         (in-hole E tt))])
  
(define-judgment-form L
  #:mode (--> I O)
  #:contract (--> (p (s ...))
                  (p (s ...)))

  [(e--> e_2 e_2p)
   ----- "e-->"
   (--> (((x_1 = e_1) ... (x_2 = e_2) (x_3 = e_3) ...)
         (s ...))
        (((x_1 = e_1) ... (x_2 = e_2p) (x_3 = e_3) ...)
         (s ...)))]

  [----- "action fire"
   (--> (((x_1 = e_1) ... (x = (in-hole E (action tt s_1))) (x_2 = e_2) ...)
         (s_2 ...))
        (((x_1 = e_1) ... (x = (in-hole E tt)) (x_2 = e_2) ...)
         (s_1 s_2 ...)))]

  [----- "action pass"
   (--> (((x_1 = e_1) ... (x = (in-hole E (action ff s_1))) (x_2 = e_2) ...)
         (s_2 ...))
        (((x_1 = e_1) ... (x = (in-hole E ff)) (x_2 = e_2) ...)
         (s_2 ...)))]

  [----- "variable"
   (--> (((x_1 = e_1) ... (x = b) (x_2 = e_2) ...)
         (s ...))
        (((x_1 = (replace x b e_1)) ... (x_2 = (replace x b e_2)) ...)
         (s ...)))])

(define-metafunction L
  replace : x b e -> e
  [(replace x b x) b]
  [(replace x_1 b x_2) x_2]
  [(replace x b (and e ...))
   (and (replace x b e) ...)]
  [(replace x b (if e_1 e_2 e_3))
   (if (replace x b e_1)
       (replace x b e_2)
       (replace x b e_3))]
  [(replace x b (action e s))
   (action (replace x b e) s)]
  [(replace x b_1 b_2) b_2])

(test-judgment-holds
 (--> (((x1 = tt)
        (y1 = x1))
       ())
      (((y1 = tt))
       ())))

(test-judgment-holds
 (--> (((x1 = (and (and y1) ff (and z))))
       ())
      (((x1 = ff))
       ())))

(traces
 -->
 (term (((x = tt)
         (x1 = (if x tt y2))
         (y1 = (action x1 "hello"))
         (x2 = (if x y1 tt))
         (y2 = (action x2 "goodbye"))
         (y = (if x y2 y1)))
        ())))

(traces
 -->
 (term (((x = ff)
         (x1 = (if x tt y2))
         (y1 = (action x1 "hello"))
         (x2 = (if x y1 tt))
         (y2 = (action x2 "goodbye"))
         (y = (if x y2 y1)))
        ())))