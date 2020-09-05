type range = float * float
type wartosc = range list

(* jesli poczatek przedzialu jest wiekszy niz koniec to je zamienia *)
let napraw (a, b) = if a > b then (b, a) else (a, b)

let wartosc_dokladnosc x p = [napraw((x *. (1. -. p /. 100.),
    x *. (1. +. p /. 100.)))]

let wartosc_od_do a b = [(a, b)]
let wartosc_dokladna a = [(a, a)]

(* przechodzac dwie listy wykonuje funkcje f zalezna od
   elementu 1. listy, akumulatora i 2. elementu listy *)
let rec iteruj_dwie_listy l1 l2 f a = match l1 with
    | [] -> a
    | h :: t -> iteruj_dwie_listy t l2 f (List.fold_left (f h) a l2)

(* czy liczba jest jedna z wartosci*)
let in_wartosc lista x =
    let in_range acc (a, b) = (x >= a && x <= b) || acc in
    List.fold_left in_range false lista

(* minimalna wartosc *)
let min_wartosc lista =
    let pom acc (a, b) = if a < acc then a else acc in
    let wynik = List.fold_left pom infinity lista in
    if wynik = infinity then nan else wynik

(* maksymalna wartosc *)
let max_wartosc lista =
    let pom acc (a, b) = if b > acc then b else acc in
    let wynik = List.fold_left pom neg_infinity lista in
    if wynik = neg_infinity then nan else wynik

(* srednia wartosc *)
let sr_wartosc k = ((min_wartosc k) +. (max_wartosc k)) /. 2.

(* jesli poczatek przedzialu jest liczba ujemna to zamienia poczatek z koncem*)
let mod_range (a, b) = if a < 0. then (b, a) else (a, b)

(* wylacza ze zbioru przedzialów te które sa nieskonczone,
   kodujac jednoczesnie informacje o ich czesci wspólnej *)
let wyrzuc_infty w =
    let pom (acc, p, l) (a, b) =
        if a = neg_infinity then (acc, (if b < p then p else b), l)
        else if b = infinity then (acc, p, (if l < a then l else a))
        else ((a, b) :: acc, p, l)
    in
    List.fold_left pom ([], neg_infinity, infinity) w

(* ta funkcja przypisze wartosci niedokladnej jej klase abstrakcji *)
let znormalizuj war =
    let (w, p, l) = wyrzuc_infty war in
    let pp = if p = neg_infinity then [] else [(neg_infinity, p)] in
    let ll = if l = infinity then [] else [(l, infinity)] in
    let ww = if w = [] then [] else [(min_wartosc w, max_wartosc w)] in
    pp @ ll @ ww

(* Oblicza iloczyn dwoch przedzialów zgodny ze specyfikacja
   zadamy aby przedzialy na wejsciu nie zawieraly zera *)
let razy_range r1 acc r2 =
    let (a, b), (c,d) = mod_range r1, mod_range r2 in
    if classify_float (b *. d) = FP_nan then (0., 0.) :: acc
    else (napraw (a *. c, b *. d)) :: acc

(*rozdziela przedzial gdy zawiera 0 na dwa, po czym dopisuje do listy*)
let rozdziel acc (r1, r2) =
    if r1 < 0. && r2 > 0. then (0., r2) :: (r1, (-0.)) :: acc
    else if r1 = 0. then (0., r2) :: acc
    else if r2 = (-0.) then (r1, (-0.)) :: acc
    else (r1, r2) :: acc

(* oblicza mozliwe wartosci niedokladne dzialania
   podzielic wartosc_dokladna (-1.) wartosc_od_do a b
   oraz dopisuje do listy acc *)
let odwroc acc (a, b) =
   if a = 0. && b = 0. then acc
   else (napraw (1. /. b, 1. /. a)) :: acc

(* plus *)
let plus a b =
    (*dodaje do siebie dwa przedzialy po krancach i dolacza do listy acc*)
    let dodaj_range (a, b) acc (c, d) = (a +. c, b +. d) :: acc
    in znormalizuj (iteruj_dwie_listy a b dodaj_range [])

(* minus *)
let minus a b =
    let range_razy_minus_jeden acc (cc, dd) =
        (napraw ((-. cc), (-. dd))) :: acc in
    let odwrotnosc_b = List.fold_left range_razy_minus_jeden [] b in
    plus a odwrotnosc_b

(* razy *)
let razy a b =
    let (am, bm) = (List.fold_left rozdziel [] a,
      List.fold_left rozdziel [] b)
    in znormalizuj (iteruj_dwie_listy am bm razy_range [])

(* podzielic *)
let podzielic a b =
    let (am, bm) = (List.fold_left rozdziel [] a, List.fold_left odwroc []
      (List.fold_left rozdziel [] b))
    in znormalizuj (iteruj_dwie_listy am bm razy_range []);;
