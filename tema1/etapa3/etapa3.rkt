#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
(let  ((rev-engagements (let rev-eng-iter ((eng engagements) (res null)) (if (null? eng)
                                                                    res
                                                                    (rev-eng-iter (cdr eng) (cons (cons (cdar eng)
                                                                                                        (caar eng))
                                                                                                  res))))))                                                                                                
      (let get-unstable-couples-iter  ((L engagements) (res null))
                                      (if (null? L)
                                          res
                                          (let  ((p1 (cdar L)) (p2 (caar L)))
                                                (if (or (better-match-exists? p1 p2 (get-pref-list mpref p1) wpref engagements)
                                                        (better-match-exists? p2 p1 (get-pref-list wpref p2) mpref rev-engagements))
                                                    (get-unstable-couples-iter (cdr L) (cons (car L) res))
                                                    (get-unstable-couples-iter (cdr L) res)))))))


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let ext-loop ((f-men free-men) (stable-matches engagements))
                (if (null? f-men)
                    stable-matches
                    (let* ((m (car f-men)) (m-pref (get-pref-list mpref m)))
                          (let int-loop ((m-p m-pref))
                                            (let* ((w (car m-p)) (curr-m (get-partner stable-matches w)))
                                                  (if (equal? curr-m false)
                                                      (ext-loop (cdr f-men) (cons (cons w m) stable-matches))
                                                      (if (preferable? (get-pref-list wpref w) m curr-m)
                                                          (ext-loop (cons curr-m (cdr f-men)) (update-engagements stable-matches w m))
                                                          (int-loop (cdr m-p))))))))))


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (let  ((free-men (get-men mpref)))
        (engage free-men null mpref wpref)))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldl  (lambda (pair acc) (cons (cdr pair) (cons (car pair) acc)))
          null
          pair-list))

