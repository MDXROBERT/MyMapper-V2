#lang racket/gui

;need to download graph-lib in case you dont have it selected as a package in the package manager

(define (remove-spaces-and-lowercase str)
  (string-downcase (string-join (string-split str " ") "")))

(require graph)


(define ns (variable-reference->namespace (#%variable-reference)))

(define (string->procedure s)
  (define sym (string->symbol s))
  (eval sym ns))

(define (remove-element lst arg)
  (filter (lambda (x) (not (equal? x arg))) lst))

(define (contains-all? lst1 lst2)
  (andmap (lambda (x) (member x lst1)) lst2))

(define (remove-common-elements lst1 lst2)
  (filter (lambda (x) (not (member x lst2))) lst1))


(define findme (λ (x y) (cond
                          ((equal? 1 (length x)) '() )
                       (#t (append (list (string-join (append (filter (λ (w) (contains-all? (get-vertices (string->procedure w)) (first (filter (λ (z) (equal? (first z) (first x))) y)) )) tube ) (list (string-join (first (filter (λ (z) (equal? (first z) (first x))) y))"-> " ))) " line: "))  (findme (append (list (last (first (filter (λ (z) (equal? (first z) (first x))) y)))) (remove-common-elements x (first (filter (λ (z) (equal? (first z) (first x))) y))))  (remove-empty-lists (map (λ (h) (list-intersect (append (list (last (first (filter (λ (z) (equal? (first z) (first x))) y)))) (remove-common-elements x (first (filter (λ (z) (equal? (first z) (first x))) y)))) h)) (remove-element y (first (filter (λ (z) (equal? (first z) (first x))) y)) ) )))          )))
                ))

                                    (define (remove-empty-lists lst)
  (filter (lambda (sublist) (not (null? sublist))) lst))

(define quickfix (λ (x) (reverse (rest (reverse x)))))

(define sep (λ (x y) (send fr set-value (substring (apply string-append (map (lambda (line) (string-append line "\n↓\n")) (findme x (remove-empty-lists (map (λ (z) ( list-intersect x z)) y))))) 0 (- (string-length (apply string-append (map (lambda (line) (string-append line "\n↓\n")) (findme x (remove-empty-lists (map (λ (z) ( list-intersect x z)) y))))) ) 3 )))))


(define list-intersect (λ (x y)
                         (filter (λ (z) (member z y)) x)))

  (define dcheck (λ (s d) (cond
                            ((not (andmap (λ (x)  (has-vertex? tubeg x)) (list s d))) (send fr set-value "There is no route found"))
                            ((not (empty? (filter-map (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube))) (send fr set-value (string-join (append (cond
                                                                                                                                                                                   ((equal? (length (filter (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube)) 1) (filter (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube))
                                                                                                                                                                                   (#t (list (string-join (filter (λ (y) (andmap (λ (x)  (has-vertex? (string->procedure y) x)) (list s d))) tube) " or "))))
                                                                                                                                                                                    (list (string-join (fewest-vertices-path tubeg s d) "-> "))) " line:   "))) 
                            (#t (sep (fewest-vertices-path tubeg s d)  (map (λ (x) (list-intersect (get-vertices (string->procedure x)) (fewest-vertices-path tubeg s d))) tube))))))


(define (count-occurrences str)
  (let ([count 0])
    (for ([i (in-range (string-length str))])
      (when (char=? (string-ref str i) #\↓)
        (set! count (+ count 1))))
    count))







(define tube (list "Northen" "Bakerloo" "Victoria"))

(define tubeg (unweighted-graph/undirected '(                 
("Elephant and Castle" "Lambeth North")
("Lambeth North" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Piccadilly Circus")
("Piccadilly Circus" "Oxford Circus")
("Oxford Circus" "Regent's Park")
("Regent's Park" "Baker Street")
("Baker Street" "Marylebone")
("Marylebone" "Edgware Road")
("Edgware Road" "Paddington")
("Paddington" "Warwick Avenue")
("Warwick Avenue" "Maida Vale")
("Maida Vale" "Kilburn Park")
("Kilburn Park" "Queens Park")
("Queens Park" "Kensal Green")
("Kensal Green" "Wilesden Juction")
("Wilesden Juction" "Harlesden")
("Harlesden" "Stonebridge Park")
("Stonebridge Park" "Wembley Central")
("Wembley Central" "North Wembley")
("North Wembley" "South Kenton")
("South Kenton" "Kenton")
("Kenton" "Harrow and Wealdstone")
("Walthamstow Central" "Blackhorse Road")
("Blackhorse Road" "Tottenham Hale")
("Tottenham Hale" "Seven Sisters")
("Seven Sisters" "Finsbury Park")
("Finsbury Park" "Highbury & Islington")
("Highbury & Islington" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Warren Street")
("Warren Street" "Oxford Circus")
("Oxford Circus" "Green Park")
("Green Park" "Victoria")
("Victoria" "Pimlico")
("Pimlico" "Vauxhall")
("Vauxhall" "Stockwell")
("Stockwell" "Brixton")
("Morden" "South Wimbledon")
("South Wimbledon" "Colliers Wood")
("Colliers Wood" "Tooting Broadway")
("Tooting Broadway" "Tooting Bec")
("Tooting Bec" "Balham")
("Balham" "Clapham South")
("Clapham South" "Clapham Common")
("Clapham Common" "Clapham North")
("Clapham North" "Stockwell")
("Stockwell" "Oval")
("Oval" "Kennington")
("Kennington" "Nine Elms")
("Nine Elms" "Battersea Power Station")
("Kennington" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Leicester Square")
("Leicester Square" "Tottenham Court Road")
("Tottenham Court Road" "Goodge Street")
("Goodge Street" "Warren Street")
("Warren Street" "Euston")
("Euston" "Mornington Crescent")
("Mornington Crescent" "Camden Town")
("Camden Town" "Kentish Town") 
("Kentish Town" "Tuffnell Park")
("Tuffnell Park" "Archway")
("Archway" "Highgate")
("Highgate" "East Finchley")
("East Finchley" "Finchley Central")
("Finchley Central" "West Finchley")
("West Finchley" "Woodside Park")
("Woodside Park" "Totteridge & Whetstone")
("Totteridge & Whetstone" "High Barnet")
("Kennington" "Elephant & Castle")
("Elephant & Castle" "Borough")
("Borough" "London Bridge")
("London Bridge" "Bank")
("Bank" "Moorgate")
("Moorgate" "Old Street")
("Old Street" "Angel")
("Angel" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Camden Town")
("Camden Town" "Chalk Farm")
("Chalk Farm" "Belsize Park")
("Belsize Park" "Hampstead")
("Hampstead" "Golders Green")
("Golders Green" "Brent Cross")
("Brent Cross" "Hendon Central")
("Hendon Central" "Colindale")
("Colindale" "Burnt Oak")
("Burnt Oak" "Edgware")
                                             )))


(define Bakerloo (unweighted-graph/undirected '(
("Elephant and Castle" "Lambeth North")
("Lambeth North" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Piccadilly Circus")
("Piccadilly Circus" "Oxford Circus")
("Oxford Circus" "Regent's Park")
("Regent's Park" "Baker Street")
("Baker Street" "Marylebone")
("Marylebone" "Edgware Road")
("Edgware Road" "Paddington")
("Paddington" "Warwick Avenue")
("Warwick Avenue" "Maida Vale")
("Maida Vale" "Kilburn Park")
("Kilburn Park" "Queens Park")
("Queens Park" "Kensal Green")
("Kensal Green" "Wilesden Juction")
("Wilesden Juction" "Harlesden")
("Harlesden" "Stonebridge Park")
("Stonebridge Park" "Wembley Central")
("Wembley Central" "North Wembley")
("North Wembley" "South Kenton")
("South Kenton" "Kenton")
("Kenton" "Harrow and Wealdstone")

)   ))


(define Victoria (unweighted-graph/undirected '(("Walthamstow Central" "Blackhorse Road")
("Blackhorse Road" "Tottenham Hale")
("Tottenham Hale" "Seven Sisters")
("Seven Sisters" "Finsbury Park")
("Finsbury Park" "Highbury & Islington")
("Highbury & Islington" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Warren Street")
("Warren Street" "Oxford Circus")
("Oxford Circus" "Green Park")
("Green Park" "Victoria")
("Victoria" "Pimlico")
("Pimlico" "Vauxhall")
("Vauxhall" "Stockwell")
("Stockwell" "Brixton"))))

(define Northen (unweighted-graph/undirected '(
("Morden" "South Wimbledon")
("South Wimbledon" "Colliers Wood")
("Colliers Wood" "Tooting Broadway")
("Tooting Broadway" "Tooting Bec")
("Tooting Bec" "Balham")
("Balham" "Clapham South")
("Clapham South" "Clapham Common")
("Clapham Common" "Clapham North")
("Clapham North" "Stockwell")
("Stockwell" "Oval")
("Oval" "Kennington")
("Kennington" "Nine Elms")
("Nine Elms" "Battersea Power Station")
("Kennington" "Waterloo")
("Waterloo" "Embankment")
("Embankment" "Charing Cross")
("Charing Cross" "Leicester Square")
("Leicester Square" "Tottenham Court Road")
("Tottenham Court Road" "Goodge Street")
("Goodge Street" "Warren Street")
("Warren Street" "Euston")
("Euston" "Mornington Crescent")
("Mornington Crescent" "Camden Town")
("Camden Town" "Kentish Town") 
("Kentish Town" "Tuffnell Park")
("Tuffnell Park" "Archway")
("Archway" "Highgate")
("Highgate" "East Finchley")
("East Finchley" "Finchley Central")
("Finchley Central" "West Finchley")
("West Finchley" "Woodside Park")
("Woodside Park" "Totteridge & Whetstone")
("Totteridge & Whetstone" "High Barnet")
("Kennington" "Elephant & Castle")
("Elephant & Castle" "Borough")
("Borough" "London Bridge")
("London Bridge" "Bank")
("Bank" "Moorgate")
("Moorgate" "Old Street")
("Old Street" "Angel")
("Angel" "King's Cross St Pancras")
("King's Cross St Pancras" "Euston")
("Euston" "Camden Town")
("Camden Town" "Chalk Farm")
("Chalk Farm" "Belsize Park")
("Belsize Park" "Hampstead")
("Hampstead" "Golders Green")
("Golders Green" "Brent Cross")
("Brent Cross" "Hendon Central")
("Hendon Central" "Colindale")
("Colindale" "Burnt Oak")
("Burnt Oak" "Edgware"))))




(define (sort-strings lst)
  (sort lst string<=?))






  (define mapper (new frame%
                       [label "MyMapper"]
                       [width 1920]
                       
                       [height 1080]))
(define centre (new horizontal-pane%
                    [spacing 200]
                      [parent mapper]

                      ))
(define collumn3 (new vertical-pane%
                      [parent centre]
                      [spacing 0]
                      [vert-margin 0]
                      [alignment '(left top)]
                      [min-width 10]
                      ))
(define collumn1 (new vertical-pane%
                      [parent centre]
                      [min-width 400]	 
        
                      ))
(define collumn2 (new vertical-pane%
                      [parent centre]
                      [min-width 75]
                      ))
                       
(define start (new text-field%
                    [parent collumn1]
                    [label "From:"]))


(define switch (new button%
                          [parent collumn1]
                          [label "Switch"]
                          [callback (λ (o e)
                                      (let ([x (list (send start get-value) (send dest get-value))])
                                      (send dest set-value (first x))
                                        (send start set-value (second x))
                                        (dcheck (send start get-value) (send dest get-value))
                                     (cond
                                         ((not (andmap (λ (x)  (has-vertex? tubeg x)) (list (send start get-value) (send dest get-value)))) (send time-counter set-label "ETA: -"))
                                        (#t (send time-counter set-label (string-append "ETA: " (seconds-to-minutes (+ (* 360 (count-occurrences (send fr get-value))) (* 139 (- (length (fewest-vertices-path tubeg (send start get-value) (send dest get-value))) 1))))))))
                                        )
                                      )]))

(define dest (new text-field%
                   [label "To:"]
                   [parent collumn1]))

(define (seconds-to-minutes seconds)
  (define minutes (floor (/ seconds 60)))
  (define remaining-seconds (- seconds (* minutes 60)))
  (string-append (number->string minutes) " min " (number->string remaining-seconds) " sec"))
                         

(define find (new button%
                  [parent collumn1]
                  [label "Find Route"]
                  [callback (λ (o e) (let ([x (list (send start get-value) (send dest get-value))])
                                       (dcheck (send start get-value) (send dest get-value))
                                       (cond
                                         ((not (andmap (λ (x)  (has-vertex? tubeg x)) (list (send start get-value) (send dest get-value)))) (send time-counter set-label "ETA: -"))
                                        (#t (send time-counter set-label (string-append "ETA: " (seconds-to-minutes (+ (* 360 (count-occurrences (send fr get-value))) (* 139 (- (length (fewest-vertices-path tubeg (send start get-value) (send dest get-value))) 1))))))))
                                       ))]))

(define pane1 (new vertical-pane%
                   [parent collumn1]
                   [min-height 150]))

(define fr (new text-field%
                     [label "Route:"]
                     [min-height 150]
                     [style '(multiple)]
                     [parent pane1]
                     [enabled #f]))

(define pane2 (new vertical-pane%
                   [parent collumn1]
                   [min-height 500]))

(define time-counter (new message%
                          [parent pane2]
                          [label "ETA: -"]
                           ))
(define stations-choice (new vertical-pane%
                             [parent collumn3]
                             [vert-margin 0]))

(define lines-list (new list-box%
                      [parent stations-choice]
                      [label "Avalable Lines:"]
           	 
                      [min-height 100]
                      [choices (map (λ (x) (string-append x " line")) tube)]
                      [callback (lambda (c e) (let ([nothing 1])
                                                (send station-list set (sort-strings (get-vertices (string->procedure (list-ref tube (first (send lines-list get-selections))))))  )
                                                (send station-list select 0 #t)
                                                ))]
                      ))

(define station-list (new list-box%
                      [parent stations-choice]
                      [label "Stations:"]
                      [min-height 500]
                      [choices (sort-strings(get-vertices Northen))]))

(send lines-list select 0 #t)
(send station-list select 0 #t)

(define buttons (new horizontal-pane%
                     [alignment '(center top)]
                     [parent collumn3]
                     [vert-margin 0]
                     [min-height 200]))
  
(define make-start (new button%
                        [parent buttons]
                        [label "Make Starting Location"]
                        [callback (λ (o e) (let ([nothing 1])
                                             (send start set-value (list-ref (sort-strings (get-vertices (string->procedure (list-ref tube (first (send lines-list get-selections)))))) (first (send station-list get-selections))))
                                             (dcheck (send start get-value) (send dest get-value)))
                                  (cond
                                         ((not (andmap (λ (x)  (has-vertex? tubeg x)) (list (send start get-value) (send dest get-value)))) (send time-counter set-label "ETA: -"))
                                        (#t (send time-counter set-label (string-append "ETA: " (seconds-to-minutes (+ (* 360 (count-occurrences (send fr get-value))) (* 139 (- (length (fewest-vertices-path tubeg (send start get-value) (send dest get-value))) 1)))))))))
                                    ]))
  (define make-dest (new button%
                     [parent buttons]
                     [label "Make Destination"]
                     [callback (λ (o e) (let ([nothing 1])
                                          (send dest set-value (list-ref (sort-strings (get-vertices (string->procedure (list-ref tube (first (send lines-list get-selections)))))) (first (send station-list get-selections))))
                                          (dcheck (send start get-value) (send dest get-value)))
                               (cond
                                         ((not (andmap (λ (x)  (has-vertex? tubeg x)) (list (send start get-value) (send dest get-value)))) (send time-counter set-label "ETA: -"))
                                        (#t (send time-counter set-label (string-append "ETA: " (seconds-to-minutes (+ (* 360 (count-occurrences (send fr get-value))) (* 139 (- (length (fewest-vertices-path tubeg (send start get-value) (send dest get-value))) 1)))))))) )
                                    ]
                     ))
  

(send mapper maximize #t)
(send time-counter auto-resize #t)
(send mapper show #t)

