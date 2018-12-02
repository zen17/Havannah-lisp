(defun globalne_promenljive ()
      (setq vrednost_cvora '())
      (setq trenutno_teme '())
      (setq stek '())
      (setq pretrazena_polja '())
      (setq sledeci_covek_racunar t)
      (setq stanje '())
      (setq vrsta '())
      (setq kolona '())
      (setq lista_temena '())
      (setq dimenzija_matrice '()))

(defun stampajZnak (lista)
          (cond ((null lista) '() )
            ((equalp (car lista) 1000) (format t "  " ))
            ((equalp (car lista) 1100 ) (format t " -  " ))
            ((equalp (car lista) 1110 ) (format t " O  " ))
            ((equalp (car lista) 1111 ) (format t " X  " ))
            (t(princ (code-char (car lista ) )))))

(defun stampajRed (l)
      (cond ((null l) '())
            (t (stampajRed (cdr l)) (stampajZnak (list (car l)))   )))

(defun stampajHavanu ( brojac_ascii dimenzija m)
(cond ((= brojac_ascii (+ 65 dimenzija)) '())
      (t(stampajRed (reverse (assoc brojac_ascii m ))) (format t "~%")(stampajHavanu (1+ brojac_ascii) dimenzija m) )))
;----------------------------------------------------------------------------

;Dinamicko kreiranje matrice
;----------------------------------------------------------------------------
(defun kreirajVrstu (slovo dimenzija_matrice br_blanko)
      (append (list slovo) (kreirajListuZaVrstu dimenzija_matrice br_blanko)))

(defun kreirajListuZaVrstu (dimenzija_matrice br_blanko )
      (cond ((> dimenzija_matrice 0)
             (if ( > br_blanko 0 )
                        (append  '( 1000 ) (kreirajListuZaVrstu (1- dimenzija_matrice) (1- br_blanko))  )
                             (append '( 1100 )  (kreirajListuZaVrstu (1- dimenzija_matrice) (1- br_blanko)) ) )  )
                            (t '()) ))

(defun kalkulacijaZaVrstu (dimenzija_matrice)
     (- (* dimenzija_matrice 2) 1) )

(defun kalkulacijaZaBlanko (dimenzija_matrice)
     (- dimenzija_matrice 1) )

(defun inicijalizujStanje (brojac_ascii dimenzija_matrice br_blanko uslov )
 (cond ( (< (abs br_blanko) uslov)
      (cond (( < br_blanko 0 )
              (cons (kreirajVrstu brojac_ascii dimenzija_matrice (abs br_blanko)) (inicijalizujStanje (1+ brojac_ascii) dimenzija_matrice (1- br_blanko) uslov) ) )
                  (t (cons (kreirajVrstu brojac_ascii dimenzija_matrice br_blanko) (inicijalizujStanje (1+ brojac_ascii) dimenzija_matrice (1- br_blanko) uslov) ))))
                        (t '())))

(defun pocetno_stanje(n)
      (inicijalizujStanje 65 (kalkulacijaZaVrstu n) (kalkulacijaZaBlanko n ) (+ (kalkulacijaZaBlanko n ) 1)))
;----------------------------------------------------------------------------

(defun kalkulacija_indeksa (slovo) ;ulaz za prvi element funckje ubaci_element
      (- (char-code slovo) 65 ) )

(defun validan_potez ( el)
    (equalp 1100 el) )

(defun ubaci_element (i lista el ) ;ubacuje element u listu matrice
      (cond ((null lista) '())
            ((atom el )
              (if (and (= i 0) (validan_potez  (car lista)))
                    (progn
                    (setq sledeci_covek_racunar (not sledeci_covek_racunar))
                    (append (list el) (ubaci_element (1- i) (cdr lista) el)))
                      (append (list (car lista))  (ubaci_element (1- i) (cdr lista) el ))))
                        (t(if (= i 0)
                            (append (list el) (ubaci_element (1- i) (cdr lista) el))
                                (append (list (car lista))  (ubaci_element (1- i) (cdr lista) el ))))))

(defun odigraj_potez (slovo pozicija)
      (if sledeci_covek_racunar
            (setq stanje
             (ubaci_element (kalkulacija_indeksa slovo ) stanje (ubaci_element pozicija (assoc (char-code slovo ) stanje) 1111 )))
              (setq stanje
               (ubaci_element (kalkulacija_indeksa slovo ) stanje (ubaci_element pozicija (assoc (char-code slovo ) stanje) 1110 )))))

;--------------------------------------------------------------
;Most

(defun ispitaj_most (lista)
(cond ((null lista) '())
      (t (prog1 (most (car lista)) (ispitaj_most (cdr lista))))))

(defun vrati_vrednost (lista j);radi, prvi elemnt je slovo kreci od sledeci
  (cond ((= j -1) (car lista))
        (t (vrati_vrednost (cdr lista ) (1- j) ))))

(defun da_li_je_element_teme (cvor)
  (clan_liste cvor lista_temena))

;member radi samo za atome
(defun clan_liste (cvor lista)
  (cond ((null lista) '())
          ((equal cvor (car lista)) t)
          (t(clan_liste cvor (cdr lista)))))

(defun formiraj_cvor (i j) ;radi
  (append (list i) (list j)))

(defun ubaci_na_stek (cvor)
     (cond
        ((not (or (clan_liste cvor pretrazena_polja) (clan_liste cvor stek)))
             (setq stek (cons cvor stek)))
             (t '())))

(defun ubaci_u_pretrazena_polja (cvor)
      (if (not (clan_liste cvor pretrazena_polja))
               (setq pretrazena_polja  (cons cvor pretrazena_polja))))

(defun formiraj_listu_temena ()
  (list(append (list 65) (list (1- dimenzija_matrice )))
       (append (list 65) (list (- (* dimenzija_matrice 2) 2)))
       (append (list (+ 65 (1- dimenzija_matrice))) (list 0))
       (append (list (+ 65 (1- dimenzija_matrice))) (list (- (* dimenzija_matrice 2) 2)))
       (append (list (+ 65 (- (* dimenzija_matrice 2) 2))) (list (1- dimenzija_matrice)))
       (append (list (+ 65 (- (* dimenzija_matrice 2) 2))) (list(- (* dimenzija_matrice 2) 2)))))

(defun resetuj_promenljive_most ()
  (setq stek '())
  (setq pretrazena_polja '()))

(defun most (cvor)
    (setq trenutno_teme cvor)
    (setq vrednost_cvora (vrati_vrednost   (assoc (car cvor) stanje) (car (cdr cvor))))
    (ubaci_na_stek cvor )
    (ubaci_u_pretrazena_polja cvor)
    (print "most indikator: " )
    (print (pretrazi_matricu cvor )
    (resetuj_promenljive_most)))

(defun pretrazi_matricu (cvor)
    (cond
      ((null stek) '())
      ((and (da_li_je_element_teme (car stek))
            (not (equal trenutno_teme cvor))) (print "Most!!!!"))
      (t(prog1
               (ubaci_u_pretrazena_polja (car stek))
               (setq stek (cdr stek))
               (pretrazi_susede  (car cvor) (car(cdr cvor)))
               (pretrazi_matricu (car stek))))))

(defun pretrazi_susede (i j)
      (pretrazi_vrstu (assoc i stanje) i j -1)
      (pretrazi_vrstu (assoc (1- i) stanje ) (1- i) j -1)
      (pretrazi_vrstu (assoc (1+ i) stanje ) (1+ i) j -1))

(defun pretrazi_vrstu (lista i j brojac)
     (cond
     ((null lista) '())
     ((and(= vrednost_cvora (car lista))
          (or (= brojac j) (= brojac (- j 1)) (= brojac (+ j 1))))
      (progn
            (ubaci_na_stek (formiraj_cvor i brojac))
            (pretrazi_vrstu (cdr lista) i j (1+ brojac))))
      (t (pretrazi_vrstu (cdr lista) i j (1+ brojac) ))))

;--------------------------------------------------------------
(defun petlja ()
      (print  "Vrsta:")
      (setq vrsta (read ))
      (print  "Kolona")
      (format t "~%")
      (setq kolona (read ))
      (odigraj_potez #\A 6)
      (odigraj_potez #\E 3)
        (odigraj_potez #\A 7)
          (odigraj_potez #\E 4)
            (odigraj_potez #\A 8)
              (odigraj_potez #\E 5)
                      (odigraj_potez #\B 6)
                      (odigraj_potez #\G 3)
                        (odigraj_potez #\B 7)
                          (odigraj_potez #\G 4)
                            (odigraj_potez #\B 8)
                              (odigraj_potez #\G 5)
                                (odigraj_potez #\B 9)
                                  (odigraj_potez #\G 6)
                                    (odigraj_potez #\B 10)
                                      (odigraj_potez #\G 7)
                                      (odigraj_potez #\B 11)
                                        (odigraj_potez #\H 7)

      (odigraj_potez vrsta (1+ kolona))
      (stampajHavanu 65 (* 2 dimenzija_matrice) stanje)
      (format t "~%")
      (format t "Podaci most ~%")
      (most (formiraj_cvor 65 5))
      ;(print (ispitaj_most lista_temena))
      (petlja))

(defun start ()
      (globalne_promenljive )
      (print   "Dimenzija: ")
      (setq dimenzija_matrice ( read ) )
      (setq lista_temena (formiraj_listu_temena))
      (format t "~%")
      (setf stanje (pocetno_stanje dimenzija_matrice) )
      (stampajHavanu 65 (* 2 dimenzija_matrice) stanje)
      (petlja))
(start)
;!!!!! NAPOMENA za vrstu se unosi #\A   #\B ...
;--------------------------------------------------------------
