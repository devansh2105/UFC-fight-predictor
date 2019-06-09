#lang racket
(require csc151)

(define fighters
  (read-csv-file "/Users/devansh/Downloads/UFCFIGHTERS.csv"))

(define fighter-names
  (cdr (map car fighters)))

(define fights
  (read-csv-file "/Users/devansh/Downloads/UFCFIGHTS.csv"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ranking Key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pre Ranking is represented by the procedure pre-individual-ranking
;;; Absolute Ranking is represented by the procedure absolute-ranking-fighter
;;;    and is calculated using absolute-ranking-positive-half and absolute-ranking-negative-half
;;; Ordinal Ranking refers to ordinal counting, Ex: first, second, third, etc.
;;; Relative Ranking is calculated by relative-ranking and compares the abilities of two fighters
;;; Winning Probability determines the probability of one fighter winning when fighting another
;;;    fixed fighter and uses total-rating to determine the probability



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Here we build up the pre-ranking system ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Procedure:
;;;   filter-wins-by-name
;;; Parameters:
;;;   lst, a list of the form ("event_date" "f1name" "f2name" "f1result" "f2result" "method" "method_d" "round" "time\r")
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine if name won the fight portrayed in lst
;;; Produces:
;;;   A boolean value
(define filter-wins-by-name
  (lambda (lst name)
    (equal? name (cadr lst))))

;;; Procedure:
;;;   filter-wins-by-name-draws
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To remove draws from the list of fights won by name
;;; Produces:
;;;   result, a list of fights won by name
(define filter-wins-by-name-draws
  (lambda (name)
    (let* ([win-lst (filter (section filter-wins-by-name <> name) fights)])
      (let kernel ([lst win-lst]
                   [lst-so-far null])
        (if (null? lst)
            lst-so-far
            (if (equal? "win" (cadddr (car lst)))
                (kernel (cdr lst)
                        (cons (car lst) lst-so-far))
                (kernel (cdr lst)
                        lst-so-far)))))))

;;; Procedure:
;;;   filter-losses-by-name
;;; Parameters:
;;;   lst, a list of the form ("event_date" "f1name" "f2name" "f1result" "f2result" "method" "method_d" "round" "time\r")
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine if name lost the fight portrayed in lst
;;; Produces:
;;;   A boolean value
(define filter-losses-by-name
  (lambda (lst name)
    (equal? name (caddr lst))))

;;; Procedure:
;;;   filter-losses-by-name-draws
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To remove draws from the list of fights lost by name
;;; Produces:
;;;   result, a list of fights lost by name
(define filter-losses-by-name-draws
  (lambda (name)
    (let* ([lose-lst (filter (section filter-losses-by-name <> name) fights)])
      (let kernel ([lst lose-lst]
                   [lst-so-far null])
        (if (null? lst)
            lst-so-far
            (if (equal? "loss" (car (cddddr (car lst))))
                (kernel (cdr lst)
                        (cons (car lst) lst-so-far))
                (kernel (cdr lst)
                        lst-so-far)))))))

;;; Procedure:
;;;   total-fights-by-name
;;; Parameters:
;;;   lst, a list of the form ("event_date" "f1name" "f2name" "f1result" "f2result" "method" "method_d" "round" "time\r")
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine if name participated in the fight portrayed in lst
;;; Produces:
;;;   a boolean value
(define total-fights-by-name
  (lambda (lst name)
    (or (equal? name (cadr lst))
        (equal? name (caddr lst)))))

;;; Procedure:
;;;   proportion-of-wins
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the proportion of fights that name won
;;; Produces:
;;;   num (0 <= num <= 1) or "Never Fought" (if the fighter never fought)
(define proportion-of-wins
  (lambda (name)
    (let ([total-fights (filter (section total-fights-by-name <> name) fights)])
      (if (null? total-fights)
          "Never Fought"
          (exact->inexact (/ (length (filter-wins-by-name-draws name))
                             (length total-fights)))))))

;;; Procedure:
;;;   proportion-of-losses
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the proportion of fights that name lost
;;; Produces:
;;;   num (0 <= num <= 1) or "Never Fought" (if the fighter never fought)
(define proportion-of-losses
  (lambda (name)
    (let ([total-fights (filter (section total-fights-by-name <> name) fights)])
      (if (null? total-fights)
          "Never Fought"
          (exact->inexact (/ (length (filter-losses-by-name-draws name))
                             (length total-fights)))))))

;;; Procedure:
;;;   number-of-fights
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the number of fights fought by name
;;; Produces:
;;;   num, the number of fights fought by name
(define number-of-fights
  (lambda (name)
    (length (filter (section total-fights-by-name <> name) fights))))

;;; Procedure:
;;;   avg-round-wins
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the average number of rounds name takes to win a fight
;;; Produces:
;;;   num, the average number of rounds name takes to win a fight
(define avg-round-wins
  (lambda (name)
    (let ([wins (filter-wins-by-name-draws name)])
      (display (list 'loading)) (newline)
      (if (null? wins)
          1
          (/ (reduce + (map (section list-ref <> 7) wins))
             (length wins))))))

;;; Procedure:
;;;   avg-round-losses
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the average number of rounds name takes to lose a fight
;;; Produces:
;;;   num, the average number of rounds name takes to lose a fight
(define avg-round-losses
  (lambda (name)
    (let ([lose (filter-losses-by-name-draws name)])
      (if (null? lose)
          1
          (/ (reduce + (map (section list-ref <> 7) lose))
             (length lose))))))


;;; Procedure:
;;;   pre-individual-ranking
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the pre-ranking of name
;;; Produces:
;;;   num, the pre-ranking of name
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   pre-individual-ranking = (number of fights name) * ((proportion-of-wins name)/(avg-round-wins name)
;;;                                                        -(proportion-of-losses name)/(avg-round-losses name))
(define pre-individual-ranking
  (lambda (name)
    (if (equal? "Never Fought" (proportion-of-wins name))
        0 
        (+ 4.12
           (* (- (/  (proportion-of-wins name)
                     (avg-round-wins name))
                 (/ (proportion-of-losses name)
                    (avg-round-losses name)))
              (number-of-fights name))))))


;;; Procedure:
;;;   sort-best-ranking
;;; Parameters:
;;;   val1, a list of the form ("name" pre-individual-ranking)
;;;   val2, a list of the form ("name" pre-individual-ranking)
;;; Purpose:
;;;   To determine if the pre-ranking of val1 is greater than the pre-ranking of val2
;;; Produces:
;;;   num, the pre-ranking of name
(define sort-best-ranking
  (lambda (val1 val2)
    (> (cadr val1)
       (cadr val2))))


;;;;;;;;;;;;;;;;;;;;;;;;; Here we build up the Absolute Ranking System ;;;;;;;;;;;;;;;;;;;;;;;;
; binary-search was taken from the reading on binary search on the class website:
;   (http://www.cs.grinnell.edu/~klingeti/courses/f2017/csc151/readings/searching)
(define binary-search 
  (lambda (vec get-key may-precede? key) 
    ; Search a portion of the vector from lower-bound to upper-bound 
    (let search-portion ([lower-bound 0] 
                         [upper-bound (- (vector-length vec) 1)]) 
      ; If the portion is empty 
      (if (> lower-bound upper-bound) 
          ; Indicate the value cannot be found 
          #f 
          ; Otherwise, identify the middle point, the element at that  
          ; point and the key of that element. 
          (let* ([point-of-division (quotient (+ lower-bound upper-bound) 2)] 
                 [separating-element (vector-ref vec point-of-division)] 
                 [sep-elt-key (get-key separating-element)] 
                 [left? (may-precede? key sep-elt-key)] 
                 [right? (may-precede? sep-elt-key key)]) 
            (cond 
              ; If the middle key equals the value, we use the middle value. 
              [(and left? right?) 
               point-of-division] 
              ; If the middle key is too large, look in the left half 
              ; of the region. 
              [left? 
               (search-portion lower-bound (- point-of-division 1))] 
              ; Otherwise, the middle key must be too small, so look  
              ; in the right half of the region. 
              [else 
               (search-portion (+ point-of-division 1) upper-bound)]))))))  
 
 
;The code below turns the list of fighters into a vector containing a list with each fighter's name
(define vector-fighters 
  (list->vector (map list (sort fighter-names string<?))))  
 
;;; Procedure:
;;;   in-fights?
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine if name is included in the vector of fighter names
;;; Produces:
;;;   a boolean value or
;;;   num, a number corresponding to the name we are searching for
(define in-fights?
  (lambda (name) 
    (binary-search vector-fighters car string-ci<=? name)))  

;;; Procedure:
;;;   true?
;;; Parameters:
;;;   smth, a boolean value
;;; Purpose:
;;;   To determine if smth is true or false
;;; Produces:
;;;   a boolean value
(define true?
  (lambda (smth) 
    (not (equal? #f smth))))   

;;; Procedure:
;;;   true-name?
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine if name is included in the vector of fighter names
;;; Produces:
;;;   a boolean value
(define true-name?
  (lambda (name) 
    (true? (in-fights? name)))) 
                                                        
;;; Procedure:
;;;   fighter-wins
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the names of every fighter name beat in a fight
;;; Produces:
;;;   lst, a list of strings containing the names of every fighter name beat in a fight
(define fighter-wins
  (lambda (name) 
    (filter  true-name? 
             (map caddr (filter-wins-by-name-draws name))))) 

;;; Procedure:
;;;   fighter-losses
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the names of every fighter name lost to in a fight
;;; Produces:
;;;   lst, a list of strings containing the names of every fighter name lost to in a fight
(define fighter-losses
  (lambda (name) 
    (filter true-name? 
            (map cadr (filter-losses-by-name-draws name))))) 
 
;;; Procedure:
;;;   absolute-ranking-positive-half
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the first half of the absolute ranking of name
;;; Produces:
;;;   num, one part of the absolute ranking of name
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   absolute-ranking-positive-half = (pre-individual-ranking name)
;;;                                      + sum ((pre-individual-ranking opponent)/(pre-individual-ranking name))
;;;   In words, rank good is the individual ranking of name added to the sum of the individual ranking
;;;   each opponent divided by the in individual ranking of name
(define absolute-ranking-positive-half
  (lambda (name) 
    (let ([wins (fighter-wins name)] 
          [his-rank (pre-individual-ranking name)]) 
      (let kernel ([pos 0] 
                   [our-ranking (pre-individual-ranking name)])
        (display (list 'still-loading)) (newline)
        (cond 
          [(equal? pos (length wins)) 
           our-ranking] 
          [else 
           (kernel (+ 1 pos) 
                   (+ 
                    (/ (cadr (assoc (list-ref wins pos) sorted-ranking)) his-rank) 
                    our-ranking))]))))) 
 
;;; Procedure:
;;;   absolute-ranking-negative-half
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the second half of the absolute ranking of name
;;; Produces:
;;;   num, the second part of the absolute ranking of name
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   absolute-ranking-negative-half = sum ((pre-individual-ranking name)/(pre-individual-ranking opponent))
;;;   In words, absolute-ranking-negative-half equals the sum of the pre-individual-ranking of the fighter divided
;;;   by the individual ranking of each opponent
(define absolute-ranking-negative-half
  (lambda (name) 
    (let ([losses (fighter-losses name)] 
          [his-rank (cadr (assoc name sorted-ranking))]) 
      (let kernel ([pos 0] 
                   [our-ranking 0])
        (display (list 'still-loading)) (newline)
        (cond 
          [(equal? pos (length losses)) 
           our-ranking] 
          [else 
           (kernel (+ 1 pos) 
                   (+ 
                    (/ his-rank (cadr (assoc (list-ref losses pos) sorted-ranking))) 
                    our-ranking))]))))) 

;;; Procedure:
;;;   absolute-ranking-order
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the absolute ranking of name
;;; Produces:
;;;   num, the absolute ranking of name
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   -655 <= num <= 1946
;;;   absolute ranking = (absolute-ranking-positive-half name) - (absolute-ranking-negative-half name)
(define absolute-ranking-fighter
  (lambda (name) 
    (-(absolute-ranking-positive-half name) (absolute-ranking-negative-half name)))) 
 
;;; Procedure:
;;;   alphabetical<?
;;; Parameters:
;;;   lst1, a list where the second element is a string
;;;   lst2, a list where the second element is a string
;;; Purpose:
;;;   To sort a list of lists alphabetically by fighter name
;;; Produces:
;;;   a boolean value
(define alphabetical<?
  (lambda (lst1 lst2) 
    (string<? (cadr lst1)
              (cadr lst2))))


;;;;;;;;;;;;;;;;;; Relative Ranking System ;;;;;;;;;;;;;;;;;;;;;;

;;; Procedure:
;;;   get-age
;;; Parameters:
;;;   date, a string of the form MM/DD/YY
;;; Purpose:
;;;   To determine a player's age
;;; Produces:
;;;   num, a fighter's age
(define get-age 
  (lambda (date) 
    (let ([list-date (string->list date)]) 
      (- 117 
         (list->number 
          (reverse (take (reverse list-date) 
                         2)))))))

;;; Procedure:
;;;   list->number
;;; Parameters:
;;;   year, a list of characters which represent numbers
;;; Purpose:
;;;   To convert a fighter's age from a list to a number
;;; Produces:
;;;   num, the last two digits of the year in which a fighter was born
(define list->number 
  (lambda (year) 
    (string->number (reduce string-append 
                            (map (o number->string 
                                    (section - <> 48) 
                                    char->integer) year))))) 

;These are the proportions we assigned to each aspect of relative ranking
; pH + pW + pA + pAR = 1
; pH corresponds to height
; pW corresponds to weight
; pA corresponds to age
; pAR corresponds to absolute ranking
(define pH 0.1) 
(define pW 0.3) 
(define pA 0.1) 
(define pAR 0.5) 

;;; Procedure:
;;;   total-rating
;;; Parameters:
;;;   f1, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the total rating of a fighter
;;; Produces:
;;;   num, a number relaying the total rating of f1
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   Total rating takes into account the height, weight, age, and absolute ranking of f1
(define total-rating 
  (lambda (f1) 
    (let ([data-f1 (assoc f1 fighters)]) 
      (+ (* pH (caddr data-f1)) 
         (* pW (cadddr data-f1)) 
         (* (- 0 pA) (get-age (cadr data-f1)))  ;Greater age is worse hence negative 
         (* pAR (absolute-ranking-fighter f1)))))) 

;;; Procedure:
;;;   relative-ranking
;;; Parameters:
;;;   f1, a string containing the name of a UFC fighter
;;;   f2, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To compare the abilities of f1 and f2
;;; Produces:
;;;   num, a number relating the abilities of f1 and f2
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   If num > 0, f2 would win a hypothetical fight between f1 and f2
;;;   If num < 0, f1 would win a hypothetical fight between f1 and f2
(define relative-ranking 
  (lambda (f1 f2) 
      (- (total-rating f2)
         (total-rating f1))))


;;; Procedure:
;;;   UFC-fight
;;; Parameters:
;;;   f1, a string containing the name of a UFC fighter
;;;   f2, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine which fighter would win in a hypothetical fight
;;; Produces:
;;;   winner, the name of the fighter who would win in a hypothetical fight between f1 and f2
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   winner is a string depicting the winner of a hypothetical fight between f1 and f2
;;;   winner is chosen based on relative-ranking
(define UFC-fight 
  (lambda (f1 f2) 
    (if (> (relative-ranking f1 f2) 0) 
        f2 
        f1)))

;;; Procedure:
;;;   winning-probability
;;; Parameters:
;;;   f1, a string containing the name of a UFC fighter
;;;   f2, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine if f1 or f2 would win in a hypothetical fight and to determine
;;;   the probability of them winning
;;; Produces:
;;;   lst, a lst
;;; Preconditions
;;;   No additional
;;; Postconditions
;;;   lst contains the name of the fighter that would win in a hypothetical
;;;    fight based on UFC-fight
;;;   lst also contains a statement with the winning fighter's name and the probability
;;;    of that fighter winning.
(define winning-probability 
  (lambda (f1 f2) 
    (let ([v1 (total-rating f1)] 
          [v2 (total-rating f2)]) 
      (if (equal? f2 (UFC-fight f1 f2)) 
          (display (list "Probability of" f2 "winning is" (* (/ v2 (+ v2 v1)) 100) "%")) 
          (display (list "Probability of" f1 "winning is" (* (/ v1 (+ v2 v1)) 100) "%")))
      (newline)))) 


;;;;;;;;;;;;;;;;;; Various Helpful Lists ;;;;;;;;;;;;;;;;;;
; We used the below lists to assist us as we were writing the code.  Lists
; of great importance are noted with triple stars (***) and have a note as
; to their purpose


;This list gives each fighter's proportion of wins in alphabetical order
(define fighter-proportion-wins
  (map proportion-of-wins fighter-names))
;This list gives each fighter's proportion of losses in alphabetical order
(define fighter-proportion-losses
  (map proportion-of-losses fighter-names))
;This list gives a list of total number of fights in alphabetical order
(define fighter-proportion-total
  (map number-of-fights fighter-names))
;This list gives a list of avgerage rounds taken to win for each player in alphabetical order
(define avg-round-player
  (map avg-round-wins fighter-names))


;This list gives us pre-rankings in alphabetical order
(define ranking
  (map append (map list fighter-names)
       (map list (map pre-individual-ranking fighter-names))))
;This list gives us pre-rankings sorted in descending order
(define sorted-ranking
  (sort ranking sort-best-ranking))
;This list gives us pre-rankings sorted in ascending order
(define reverse-ranking
  (reverse sorted-ranking))

;The following two lists contain absolute-rankings which was one of the main goals of our project
;(***) This list gives fighter names and their associated absolute ranking in alphabetical order
(define absolute-list  
  (map append (map list fighter-names) 
       (map list (map absolute-ranking-fighter fighter-names))))
;(***) This list gives absolute rankings in descending order
(define sorted-absolute-list 
  (sort absolute-list sort-best-ranking))
;(***) This list gives absolute rankings in ascending order with the associated ordinal ranking 
(define added-ranking  
  (map append 
       (map list (map increment (iota (length sorted-absolute-list)))) 
       sorted-absolute-list))
; This vector contains lists of the form (ordinal-ranking "name of UFC fighter" absolute-ranking)
(define sorted-by-name-vec 
  (list->vector (sort added-ranking alphabetical<?)))

;;; Procedure:
;;;   ranking?
;;; Parameters:
;;;   name, a string containing the name of a UFC fighter
;;; Purpose:
;;;   To determine the ordinal ranking of name
;;; Produces:
;;;   a string of the form: "The ranking of" name " is " ordinal-ranking
(define ranking?
  (lambda (name) 
    (string-append "The ranking of " name " is " 
                   (number->string (car (vector-ref sorted-by-name-vec 
                                                    (binary-search sorted-by-name-vec cadr string-ci<=? name))))))) 

