(setq stanje '())
(setq matrica '(
         (65 "" "" "" 2 2 0 1 )
         (66 "" "" 2 2 2 1 0  ) 
         (67 "" 2 2 2 1 0 1 )
         (68 2 2 2 2 1 0 1 )
         (69 "" 2 2 2 1 1 2)
         (70 "" "" 2 2 1 0 1 )
         (71 "" "" "" 2 1 0  2) ))

;Stampanje matrice
;----------------------------------------------------------------------------
(defun stampajZnak (lista)
          (cond ((null lista) '() )
            ((equalp (car lista) "") (format t "  " ))
            ((equalp (car lista) 2 ) (format t " -  " ))
            ((equalp (car lista) 1 ) (format t " X  " ))
            ((equalp (car lista) 0 ) (format t " O  " ))
             (t (princ (code-char (car lista ) )))))

(defun stampajRed (l)
      (cond ((null l) '())
            (t (stampajRed (cdr l)) (stampajZnak (list (car l)))   )))      

(defun stampajHavanu ( indeks dimenzija m)
(cond ((= indeks (+ 65 dimenzija)) '())
      (t(stampajRed (reverse (assoc indeks m ))) 
         (format t "~%")
          (stampajHavanu (1+ indeks) dimenzija m) )))

(stampajHavanu 65 8 matrica)
;----------------------------------------------------------------------------

(defun kreirajVrstu (slovo dimenzija_matrice br_blanko) 
      (append (list slovo) (kreirajListuZaVrstu dimenzija_matrice br_blanko)))


(defun kreirajListuZaVrstu (dimenzija_matrice br_blanko )
      (cond ((> dimenzija_matrice 0)
             (if ( > br_blanko 0 ) 
                        (append  '( "" ) (kreirajListuZaVrstu (1- dimenzija_matrice) (1- br_blanko))  )
                             (append '(-)  (kreirajListuZaVrstu (1- dimenzija_matrice) (1- br_blanko)) ) )  )
                            (t '()) ))




(defun inicijalizuj1 ( i dimenzija k )
      (cond ((< dimenzija 0) '())
                 (t (cons (kreirajVrstu i dimenzija k ) (inicijalizuj1 (1+ i) dimenzija  (1- k)) ) )
                 ))

 
(defun kalkulacijaZaVrstu (dimenzija_matrice)
     (- (* dimenzija_matrice 2) 1) ) 

(defun kalkulacijaZaBlanko (dimenzija_matrice)
     (- dimenzija_matrice 1) ) 



(defun inicijalizujStanje (indeks dimenzija_matrice br_blanko uslov )
 (cond ( (< (abs br_blanko) uslov) 
      (cond (( < br_blanko 0 )
              (cons (kreirajVrstu indeks dimenzija_matrice (abs br_blanko)) (inicijalizujStanje (1+ indeks) dimenzija_matrice (1- br_blanko) uslov) ) )
                  (t (cons (kreirajVrstu indeks dimenzija_matrice br_blanko) (inicijalizujStanje (1+ indeks) dimenzija_matrice (1- br_blanko) uslov) ))))
                        (t '())))

     
(defun init(n)
      (inicijalizujStanje 65 (kalkulacijaZaVrstu n) (kalkulacijaZaBlanko n ) (+ (kalkulacijaZaBlanko n ) 1)))


(format t "~%")
(princ (init 10)) ;65 + br_blanko +1
(format t "~%")